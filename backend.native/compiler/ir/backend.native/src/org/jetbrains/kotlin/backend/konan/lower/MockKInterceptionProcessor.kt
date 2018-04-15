package org.jetbrains.kotlin.backend.konan.lower

import org.jetbrains.kotlin.backend.common.BackendContext
import org.jetbrains.kotlin.backend.common.ir.addSimpleDelegatingConstructor
import org.jetbrains.kotlin.backend.common.ir.createSimpleDelegatingConstructorDescriptor
import org.jetbrains.kotlin.backend.common.lower.*
import org.jetbrains.kotlin.backend.common.reportWarning
import org.jetbrains.kotlin.backend.konan.Context
import org.jetbrains.kotlin.backend.konan.descriptors.findPackage
import org.jetbrains.kotlin.backend.konan.descriptors.synthesizedName
import org.jetbrains.kotlin.backend.konan.irasdescriptors.fqNameSafe
import org.jetbrains.kotlin.backend.konan.irasdescriptors.name
import org.jetbrains.kotlin.backend.konan.llvm.localHash
import org.jetbrains.kotlin.backend.konan.llvm.privateClassesTableSymbolName
import org.jetbrains.kotlin.backend.konan.objcexport.getErasedTypeClass
import org.jetbrains.kotlin.backend.konan.reportCompilationError
import org.jetbrains.kotlin.builtins.createFunctionType
import org.jetbrains.kotlin.descriptors.*
import org.jetbrains.kotlin.descriptors.annotations.Annotations
import org.jetbrains.kotlin.descriptors.impl.*
import org.jetbrains.kotlin.incremental.components.NoLookupLocation
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.UNDEFINED_OFFSET
import org.jetbrains.kotlin.ir.backend.js.utils.kind
import org.jetbrains.kotlin.ir.builders.*
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.declarations.impl.*
import org.jetbrains.kotlin.ir.expressions.IrBlockBody
import org.jetbrains.kotlin.ir.expressions.IrBody
import org.jetbrains.kotlin.ir.expressions.IrStatementOrigin
import org.jetbrains.kotlin.ir.expressions.impl.*
import org.jetbrains.kotlin.ir.symbols.IrFieldSymbol
import org.jetbrains.kotlin.ir.symbols.IrFunctionSymbol
import org.jetbrains.kotlin.ir.symbols.IrSimpleFunctionSymbol
import org.jetbrains.kotlin.ir.symbols.impl.*
import org.jetbrains.kotlin.ir.util.*
import org.jetbrains.kotlin.ir.visitors.IrElementVisitorVoid
import org.jetbrains.kotlin.ir.visitors.acceptChildrenVoid
import org.jetbrains.kotlin.ir.visitors.acceptVoid
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.resolve.scopes.MemberScope
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.Variance
import kotlin.math.abs

internal class MockKInterceptionProcessor(val context: Context) {
    object MOCKK_GENERATED : IrDeclarationOriginImpl("MOCKK_GENERATED")

    private lateinit var symbols: MockKSymbols

    inner class MockKSymbols {
        private val interceptorClassDescriptor = resolveClassDescriptor(
            FqName.fromSegments(
                listOf(
                    "io",
                    "mockk",
                    "impl",
                    "interception",
                    "Interceptor"
                )
            )
        )

        val interceptorClassSymbol = IrClassSymbolImpl(interceptorClassDescriptor)

        private val interceptFunctionDescriptor = interceptorClassDescriptor.unsubstitutedMemberScope
            .getContributedFunctions(Name.identifier("intercept"), NoLookupLocation.FROM_BACKEND)
            .singleOrNull() ?: throw IllegalStateException("intercept")

        val interceptFunctionSymbol = createFunctionSymbol(interceptFunctionDescriptor)

        private val passThroughInterceptorObjectDescriptor = resolveClassDescriptor(
            FqName.fromSegments(
                listOf(
                    "io",
                    "mockk",
                    "impl",
                    "interception",
                    "PassThroughInterceptor"
                )
            )
        )

        val passThroughInterceptorObjectSymbol = IrClassSymbolImpl(passThroughInterceptorObjectDescriptor)

        val interceptorsClassDescriptor = resolveClassDescriptor(
            FqName.fromSegments(
                listOf(
                    "io",
                    "mockk",
                    "impl",
                    "interception",
                    "Interceptors"
                )
            )
        )

        val interceptorsClassSymbol = IrClassSymbolImpl(interceptorsClassDescriptor)

        private val registerFunctionDescriptor = interceptorsClassDescriptor.unsubstitutedMemberScope
            .getContributedFunctions(Name.identifier("register"), NoLookupLocation.FROM_BACKEND)
            .singleOrNull() ?: throw IllegalStateException("register")

        val registerFunctionSymbol = createFunctionSymbol(registerFunctionDescriptor)


        private val interceptorOperationClassDescriptor = resolveClassDescriptor(
            FqName.fromSegments(
                listOf(
                    "io",
                    "mockk",
                    "impl",
                    "interception",
                    "InterceptorOperation"
                )
            )
        )

        val interceptorOperationClassSymbol = IrClassSymbolImpl(interceptorOperationClassDescriptor)

        private fun resolveClassDescriptor(name: FqName): ClassDescriptor {
            return context.moduleDescriptor.resolveClassByFqName(
                name,
                NoLookupLocation.FROM_BACKEND
            ) as ClassDescriptor
        }

        private val applyOperationFunctionDescriptor = interceptorOperationClassDescriptor
            .unsubstitutedMemberScope
            .getContributedFunctions(Name.identifier("applyOperation"), NoLookupLocation.FROM_BACKEND)
            .singleOrNull() ?: throw IllegalStateException("applyOperation")

        val applyOperationFunctionSymbol = createFunctionSymbol(applyOperationFunctionDescriptor)
    }


    fun process(irModuleFragment: IrModuleFragment) {
        symbols = try {
            MockKSymbols()
        } catch (ex: Exception) {
            context.reportWarning(
                "MockK classes not found: ${ex.message}", irModuleFragment.files.first(), irModuleFragment
            )
            return
        }

        irModuleFragment.descriptor.allDependencyModules.first().privateClassesTableSymbolName


        irModuleFragment.files.forEach { file ->
            val collector = FunctionCollector()
            collector.visitElement(file)

            (collector.otherFunctions /*+ collector.topLevelFunctions */)
                .filter { it is IrSimpleFunction }
                .filterNot {
                    it.findPackage()
                        .fqName
                        .startsWith(
                            Name.identifier("io.mockk")
                        )
                }.forEach { fn ->
                    val interceptorProperty = addInterceptorProperty(fn, file)
                    fn.body = wrapInterceptorCall(fn,
                        interceptorProperty.parent as IrClass,
                        interceptorProperty)
                }

            println(file.dump())
        }
    }

    private fun wrapInterceptorCall(
        fn: IrFunction,
        interceptorMapping: IrClass,
        interceptorProperty: IrProperty
    ): IrBody {
        return with(context.createIrBuilder(fn.symbol)) {
            irExprBody(
                irCall(symbols.interceptFunctionSymbol, listOf(fn.returnType)).apply {
                    val getter = interceptorProperty.getter!!

                    dispatchReceiver = IrCallImpl(
                        UNDEFINED_OFFSET,
                        UNDEFINED_OFFSET,
                        getter.returnType,
                        getter.symbol,
                        getter.descriptor,
                        0,
                        IrStatementOrigin.GET_PROPERTY
                    ).apply {
                        dispatchReceiver = IrGetObjectValueImpl(
                            UNDEFINED_OFFSET,
                            UNDEFINED_OFFSET,
                            interceptorMapping.defaultType,
                            interceptorMapping.symbol
                        )
                    }

                    putValueArgument(0,
                        IrVarargImpl(
                            UNDEFINED_OFFSET,
                            UNDEFINED_OFFSET,
                            context.builtIns.getArrayType(Variance.INVARIANT, context.builtIns.nullableAnyType),
                            context.builtIns.nullableAnyType,
                            fn.valueParameters.map {
                                IrGetValueImpl(
                                    UNDEFINED_OFFSET,
                                    UNDEFINED_OFFSET,
                                    it.symbol
                                )
                            }
                        )
                    )
                    putValueArgument(1,
                        irBlock(
                            resultType =
                            fn0(fn.returnType),
                            origin = IrStatementOrigin.LAMBDA
                        ) {
                            buildAndReferenceInterceptLambda(fn)
                        }
                    )
                }
            )
        }
    }

    private fun IrBlockBuilder.buildAndReferenceInterceptLambda(fn: IrFunction) {
        val fnBody = fn.body!!

        val inteceptLambda = IrFunctionImpl(
            fn.startOffset,
            fn.endOffset,
            IrDeclarationOrigin.LOCAL_FUNCTION_FOR_LAMBDA,
            buildInterceptBlockLambda(fn)
        ).apply {

            val remapper = object : DeepCopySymbolsRemapper() {
                override fun getReferencedFunction(symbol: IrFunctionSymbol): IrFunctionSymbol {
                    if (symbol.descriptor == fn.descriptor) {
                        return this@apply.symbol
                    }
                    return symbol
                }
            }

            fnBody.acceptVoid(remapper)

            body = fnBody
                .transform(DeepCopyIrTreeWithSymbols(remapper), null)
                .patchDeclarationParents()
        }

        +inteceptLambda

        +IrFunctionReferenceImpl(
            UNDEFINED_OFFSET,
            UNDEFINED_OFFSET,
            fn0(context.builtIns.unitType),
            inteceptLambda.symbol,
            inteceptLambda.descriptor,
            0,
            IrStatementOrigin.LAMBDA
        )
    }

    private fun addInterceptorProperty(
        fn: IrFunction,
        file: IrFile
    ): IrProperty {
        val interceptorMappingObject = getOrCreateInterceptorMappingObject(fn.parent, file)

        val interceptorProperty = createInterceptorProperty(fn, interceptorMappingObject)
        appendUpdate(interceptorMappingObject, fn, interceptorProperty)

        interceptorMappingObject.addChild(interceptorProperty)

        return interceptorProperty
    }

    private fun appendUpdate(
        mappingObject: IrClass,
        fn: IrFunction,
        property: IrProperty
    ) {
        val updateFunction = getFunctionByName(
            mappingObject,
            Name.identifier("update")
        ) ?: throw IllegalStateException("missing 'update' function")

        updateFunction as IrFunctionImpl
        val block = updateFunction.body as IrBlockBody

        block.statements.add(
            with(context.createIrBuilder(updateFunction.symbol)) {
                irCall(property.setter!!.symbol).apply {
                    dispatchReceiver = irGet(updateFunction.dispatchReceiverParameter!!.symbol)

                    putValueArgument(
                        0,
                        irCall(symbols.applyOperationFunctionSymbol).apply {
                            dispatchReceiver = irGet(updateFunction.valueParameters[0].symbol)
                            putValueArgument(0,
                                irCall(property.getter!!.symbol).apply {
                                    dispatchReceiver = irGet(updateFunction.dispatchReceiverParameter!!.symbol)
                                }
                            )
                            putValueArgument(
                                1,
                                IrConstImpl.string(
                                    UNDEFINED_OFFSET,
                                    UNDEFINED_OFFSET,
                                    context.builtIns.stringType,
                                    fn.name.asString()
                                )
                            )
                            val kClass = symbols
                                .applyOperationFunctionSymbol
                                .descriptor
                                .valueParameters[2]
                                .varargElementType!!

                            putValueArgument(
                                2,
                                IrVarargImpl(
                                    UNDEFINED_OFFSET,
                                    UNDEFINED_OFFSET,
                                    context.builtIns.getArrayType(Variance.INVARIANT, kClass),
                                    kClass,
                                    fn.valueParameters.map {
                                        IrClassReferenceImpl(
                                            UNDEFINED_OFFSET,
                                            UNDEFINED_OFFSET,
                                            kClass,
                                            IrClassSymbolImpl(it.type.getErasedTypeClass()),
                                            it.type
                                        )
                                    }
                                )
                            )
                        }
                    )
                }
            }
        )
    }

    private fun getFunctionByName(
        mappingObject: IrClass,
        name: Name
    ): IrFunction? {
        val visitor = object : IrElementVisitorVoid {
            var result: IrFunction? = null

            override fun visitElement(element: IrElement) {
                if (element is IrFunction && element.name == name) {
                    result = element
                }
            }
        }
        mappingObject.acceptChildren(visitor, null)
        return visitor.result
    }

    private fun createInterceptorProperty(
        fn: IrFunction,
        interceptorMappingObject: IrClass
    ): IrProperty {
        val propertyName = nameWithSignatureHash(fn).synthesizedName

        val propertyBuilder = context.createPropertyWithBackingFieldBuilder(
            UNDEFINED_OFFSET,
            UNDEFINED_OFFSET,
            MOCKK_GENERATED,
            interceptorMappingObject.descriptor,
            propertyName,
            symbols.interceptorClassSymbol.descriptor.defaultType,
            true
        )

        propertyBuilder.initialize()

        val interceptorProperty = propertyBuilder.ir

        interceptorProperty.backingField!!.initializer =
                IrExpressionBodyImpl(
                    IrGetObjectValueImpl(
                        UNDEFINED_OFFSET,
                        UNDEFINED_OFFSET,
                        symbols.passThroughInterceptorObjectSymbol.descriptor.defaultType,
                        symbols.passThroughInterceptorObjectSymbol
                    )
                )
        return interceptorProperty
    }

    private fun hash(str: String): String {
        val hash = abs(str.localHash.value)
            .let { if (it <= 0) 1 else it }

        val abc = ('A'..'Z').toList() +
                ('a'..'z').toList() +
                ('0'..'9').toList()

        var rem = hash
        val builder = StringBuilder()
        while (rem != 0L) {
            builder.append(abc[((rem % abc.size.toLong()).toInt())])
            rem /= abc.size.toLong()
        }
        return builder.toString()
    }

    private fun nameWithSignatureHash(fn: IrFunction): String {
        val prefix = fn.name.asString().replace(Regex("[^A-Za-z0-9]"), "")

        val signature = fn.name.asString() + ":" +
                fn.valueParameters
                    .map { it.type.getErasedTypeClass() }
                    .map { it.name.asString() }
                    .joinToString(", ")

        return prefix + "_" + hash(signature)
    }

    private fun getOrCreateInterceptorMappingObject(
        parent: IrDeclarationParent,
        file: IrFile
    ): IrClass {

        if (parent !is IrDeclaration) {
            context.reportCompilationError(
                "bad function parent: IrDeclaration"
            )
            throw IllegalStateException(
                "bad function parent: IrDeclaration"
            )
        }

        if (parent !is IrDeclarationContainer) {
            context.reportCompilationError(
                "bad function parent: IrDeclarationContainer"
            )
            throw IllegalStateException(
                "bad function parent: IrDeclarationContainer"
            )
        }

        val name = "IM".synthesizedName

        val foundItem = parent
            .declarations
            .firstOrNull { it.name == name }

        if (foundItem != null) {
            return foundItem as IrClass
        }

        val interceptorMapping = createInterceptorMappingObject(parent, name)

        parent.addChild(interceptorMapping)

        val updateFunction = createUpdateFunction(interceptorMapping)
        val registerFunction = createRegisterFunction(
            interceptorMapping,
            parent,
            updateFunction
        )
        file.addTopLevelInitializer(
            IrCallImpl(
                UNDEFINED_OFFSET,
                UNDEFINED_OFFSET,
                registerFunction.returnType,
                registerFunction.symbol,
                registerFunction.descriptor,
                0
            ).apply {
                dispatchReceiver = IrGetObjectValueImpl(
                    UNDEFINED_OFFSET,
                    UNDEFINED_OFFSET,
                    interceptorMapping.defaultType,
                    interceptorMapping.symbol
                )
            }
        )

        interceptorMapping.addChild(registerFunction)
        interceptorMapping.addChild(updateFunction)
        interceptorMapping.addFakeOverrides()


        return interceptorMapping
    }

    private fun createInterceptorMappingObject(
        parent: IrDeclaration,
        name: Name
    ): IrClassImpl {
        val interceptorMappingDescriptor = ClassDescriptorImpl(
            parent.descriptor,
            name,
            Modality.FINAL,
            ClassKind.OBJECT,
            listOf(context.builtIns.anyType),
            SourceElement.NO_SOURCE,
            false
        )

        val anyConstructor = context.builtIns.any.constructors.single()

        val constructorDescriptor =
            interceptorMappingDescriptor.createSimpleDelegatingConstructorDescriptor(
                anyConstructor,
                true
            )

        interceptorMappingDescriptor.initialize(
            MemberScope.Empty,
            emptySet(),
            null
        )

        return IrClassImpl(
            UNDEFINED_OFFSET,
            UNDEFINED_OFFSET,
            MOCKK_GENERATED,
            interceptorMappingDescriptor
        ).apply {
            createParameterDeclarations()

            addSimpleDelegatingConstructor(
                IrConstructorSymbolImpl(
                    anyConstructor
                ),
                constructorDescriptor,
                MOCKK_GENERATED
            )

            addFakeOverrides()
        }
    }

    private fun createRegisterFunction(
        interceptorMappingObject: IrClass,
        interceptedClassOrFile: IrDeclarationParent,
        updateFunction: IrFunction
    ): IrFunction {
        val descriptor = SimpleFunctionDescriptorImpl.create(
            interceptorMappingObject.descriptor,
            Annotations.EMPTY,
            Name.identifier("register"),
            CallableMemberDescriptor.Kind.DECLARATION,
            SourceElement.NO_SOURCE
        )

        descriptor.initialize(
            null,
            interceptorMappingObject.descriptor.thisAsReceiverParameter,
            listOf(),
            listOf(),
            context.builtIns.unitType,
            Modality.FINAL,
            Visibilities.INTERNAL
        )

        return IrFunctionImpl(
            UNDEFINED_OFFSET,
            UNDEFINED_OFFSET,
            MOCKK_GENERATED,
            descriptor
        ).apply {
            createParameterDeclarations()

            val thisReceiver = this.dispatchReceiverParameter!!.symbol

            body = IrBlockBodyImpl(
                UNDEFINED_OFFSET,
                UNDEFINED_OFFSET,
                listOf(
                    with(context.createIrBuilder(this.symbol)) {
                        irCall(
                            symbols.registerFunctionSymbol,
                            context.builtIns.unitType
                        ).apply {

                            dispatchReceiver = IrGetObjectValueImpl(
                                startOffset,
                                endOffset,
                                symbols.interceptorsClassDescriptor.defaultType,
                                symbols.interceptorsClassSymbol
                            )

                            putValueArgument(
                                0,
                                IrConstImpl.string(
                                    UNDEFINED_OFFSET,
                                    UNDEFINED_OFFSET,
                                    context.builtIns.stringType,
                                    interceptedClassOrFile.fqNameSafe.asString()
                                )
                            )

                            putValueArgument(
                                1,
                                IrFunctionReferenceImpl(
                                    UNDEFINED_OFFSET,
                                    UNDEFINED_OFFSET,
                                    fn1(
                                        symbols.interceptorOperationClassSymbol.descriptor.defaultType,
                                        context.builtIns.unitType
                                    ),
                                    updateFunction.symbol,
                                    updateFunction.descriptor
                                ).apply {
                                    dispatchReceiver = irGet(thisReceiver)
                                }
                            )

                        }
                    }
                )
            )
        }
    }

    private fun createUpdateFunction(interceptorMappingObject: IrClass): IrFunction {
        val updateFunctionDescriptor = SimpleFunctionDescriptorImpl.create(
            interceptorMappingObject.descriptor,
            Annotations.EMPTY,
            Name.identifier("update"),
            CallableMemberDescriptor.Kind.DECLARATION,
            SourceElement.NO_SOURCE
        )

        val operationParameter = ValueParameterDescriptorImpl(
            updateFunctionDescriptor,
            null,
            0,
            Annotations.EMPTY,
            Name.identifier("operation"),
            symbols.interceptorOperationClassSymbol.descriptor.defaultType,
            false,
            false,
            false,
            null,
            SourceElement.NO_SOURCE
        )

        updateFunctionDescriptor.initialize(
            null,
            interceptorMappingObject.descriptor.thisAsReceiverParameter,
            listOf(),
            listOf(operationParameter),
            context.builtIns.unitType,
            Modality.FINAL,
            Visibilities.PRIVATE
        )

        return IrFunctionImpl(
            UNDEFINED_OFFSET,
            UNDEFINED_OFFSET,
            MOCKK_GENERATED,
            updateFunctionDescriptor
        ).apply {
            body = IrBlockBodyImpl(
                UNDEFINED_OFFSET,
                UNDEFINED_OFFSET
            )

            createParameterDeclarations()
        }
    }

    private fun buildInterceptBlockLambda(fn: IrFunction): FunctionDescriptor {
        val interceptBlockLambda = AnonymousFunctionDescriptor(
            fn.descriptor.containingDeclaration,
            Annotations.EMPTY,
            CallableMemberDescriptor.Kind.DECLARATION,
            SourceElement.NO_SOURCE,
            false
        )

        interceptBlockLambda.initialize(
            /* receiverParameterType        = */ null,
            /* dispatchReceiverParameter    = */ null,
            /* typeParameters               = */ listOf(),
            /* unsubstitutedValueParameters = */ listOf(),
            /* unsubstitutedReturnType      = */ fn.returnType,
            /* modality                     = */ Modality.FINAL,
            /* visibility                   = */ Visibilities.LOCAL
        )
        return interceptBlockLambda
    }

    private fun fn0(returnType: KotlinType) = createFunctionType(
        context.builtIns,
        Annotations.EMPTY,
        null,
        listOf(),
        null,
        returnType
    )

    private fun fn1(paramType: KotlinType, returnType: KotlinType) = createFunctionType(
        context.builtIns,
        Annotations.EMPTY,
        null,
        listOf(paramType),
        null,
        returnType
    )


    private inner class FunctionCollector : IrElementVisitorVoid {
        val topLevelFunctions = mutableListOf<IrFunction>()
        val otherFunctions = mutableListOf<IrFunction>()

        override fun visitElement(element: IrElement) {
            element.acceptChildrenVoid(this)
        }

        override fun visitFunction(declaration: IrFunction) {
            val owner = declaration.descriptor.containingDeclaration

            if (declaration.symbol.kind == CallableMemberDescriptor.Kind.FAKE_OVERRIDE) {
                return
            }

            when (owner) {
                is PackageFragmentDescriptor -> topLevelFunctions.add(declaration)
                is ClassDescriptor -> otherFunctions.add(declaration)
                else -> UnsupportedOperationException("Cannot process with mocking processor function $declaration (defined in $owner")
            }
        }
    }
}

/* original code

fun main(args: Array<String>) {
    println("Hello world!")
}

class Test {
    fun abc(a: Int, b: Int) = a + b
}
 */

/* tranformed code

fun main(args: Array<String>) {
    IM.main.intercept(args) {
        println("Hello world!")
    }
}

object IM {
    var main: Interceptor = PassThroughInterceptor

    fun update(op: InterceptorOperation) {
        main = op.applyOperation(main, "main", Array<String>::class)
    }

    fun register() {
        Interceptors.register("RunKt", this::update)
    }
}

private val RunKtRegisterer = IM.register()

class Test {
    fun abc(a: Int, b: Int) = IM.abc.intercept(a, b) { a + b }

    object IM {
        var abc: Interceptor = PassThroughInterceptor

        fun update(op: InterceptorOperation) {
            abc = op.applyOperation(abc, "abc", Int::class, Int::class)
        }

        fun register() {
                Interceptors.register(Test::class.qualifiedName!!, this::update)
        }
    }
}

private val TestRegisterer = Test.IM.register()

 */


/* IR

    FUN name:main visibility:public modality:FINAL <> (args:kotlin.Array<kotlin.String>) returnType:Unit flags:
      VALUE_PARAMETER name:args index:0 type:kotlin.Array<kotlin.String> flags:
      BLOCK_BODY
        CALL 'intercept(vararg Any?, () -> Unit): Unit' type=kotlin.Unit origin=null
          <T>: Unit
          $this: CALL '<get-main>(): Interceptor' type=io.mockk.impl.interception.Interceptor origin=GET_PROPERTY
            $this: GET_OBJECT 'IM' type=IM
          args: VARARG type=Array<out Any?> varargElementType=Any?
            GET_VAR 'value-parameter args: Array<String>' type=kotlin.Array<kotlin.String> origin=null
          block: BLOCK type=() -> kotlin.Unit origin=LAMBDA
            FUN LOCAL_FUNCTION_FOR_LAMBDA name:<anonymous> visibility:local modality:FINAL <> () returnType:Unit flags:
              BLOCK_BODY
                RETURN type=kotlin.Nothing from='<anonymous>(): Unit'
                  CALL 'println(String): Unit' type=kotlin.Unit origin=null
                    message: CONST String type=kotlin.String value=Hello world!
            FUNCTION_REFERENCE '<anonymous>(): Unit' type=() -> kotlin.Unit origin=LAMBDA

    CLASS OBJECT name:IM modality:FINAL visibility:public flags:
      $this: VALUE_PARAMETER INSTANCE_RECEIVER name:<this> type:IM flags:
      superClasses:
        CLASS IR_EXTERNAL_DECLARATION_STUB CLASS name:Any modality:OPEN visibility:public flags:
      CONSTRUCTOR visibility:private <> () returnType:IM flags:
        BLOCK_BODY
          DELEGATING_CONSTRUCTOR_CALL 'constructor Any()'
          INSTANCE_INITIALIZER_CALL classDescriptor='IM'

      PROPERTY name:main type:io.mockk.impl.interception.Interceptor visibility:public modality:FINAL flags:var
        FIELD PROPERTY_BACKING_FIELD name:main type:io.mockk.impl.interception.Interceptor visibility:public
          EXPRESSION_BODY
            GET_OBJECT 'PassThroughInterceptor' type=io.mockk.impl.interception.PassThroughInterceptor
        FUN DEFAULT_PROPERTY_ACCESSOR name:<get-main> visibility:public modality:FINAL <> ($this:IM) returnType:Interceptor flags:
          $this: VALUE_PARAMETER name:<this> type:IM flags:
          BLOCK_BODY
            RETURN type=kotlin.Nothing from='<get-main>(): Interceptor'
              GET_FIELD 'main: Interceptor' type=io.mockk.impl.interception.Interceptor origin=null
                receiver: GET_VAR 'this@IM: IM' type=IM origin=null
        FUN DEFAULT_PROPERTY_ACCESSOR name:<set-main> visibility:public modality:FINAL <> ($this:IM, <set-?>:io.mockk.impl.interception.Interceptor) returnType:Unit flags:
          $this: VALUE_PARAMETER name:<this> type:IM flags:
          VALUE_PARAMETER name:<set-?> index:0 type:io.mockk.impl.interception.Interceptor flags:
          BLOCK_BODY
            SET_FIELD 'main: Interceptor' type=kotlin.Unit origin=null
              receiver: GET_VAR 'this@IM: IM' type=IM origin=null
              value: GET_VAR 'value-parameter <set-?>: Interceptor' type=io.mockk.impl.interception.Interceptor origin=null

      FUN name:update visibility:public modality:FINAL <> ($this:IM, op:io.mockk.impl.interception.InterceptorOperation) returnType:Unit flags:
        $this: VALUE_PARAMETER name:<this> type:IM flags:
        VALUE_PARAMETER name:op index:0 type:io.mockk.impl.interception.InterceptorOperation flags:
        BLOCK_BODY
          CALL '<set-main>(Interceptor): Unit' type=kotlin.Unit origin=EQ
            $this: GET_VAR 'this@IM: IM' type=IM origin=null
            <set-?>: CALL 'applyOperation(Interceptor, String, vararg KClass<*>): Interceptor' type=io.mockk.impl.interception.Interceptor origin=null
              $this: GET_VAR 'value-parameter op: InterceptorOperation' type=io.mockk.impl.interception.InterceptorOperation origin=null
              interceptor: CALL '<get-main>(): Interceptor' type=io.mockk.impl.interception.Interceptor origin=GET_PROPERTY
                $this: GET_VAR 'this@IM: IM' type=IM origin=null
              name: CONST String type=kotlin.String value=main
              types: VARARG type=Array<out KClass<*>> varargElementType=KClass<*>
                CLASS_REFERENCE 'Array' type=kotlin.reflect.KClass<kotlin.Array<kotlin.String>>
      FUN name:register visibility:public modality:FINAL <> ($this:IM) returnType:Unit flags:
        $this: VALUE_PARAMETER name:<this> type:IM flags:
        BLOCK_BODY
          CALL 'register(String, (InterceptorOperation) -> Unit): Unit' type=kotlin.Unit origin=null
            $this: GET_OBJECT 'Interceptors' type=io.mockk.impl.interception.Interceptors
            className: CONST String type=kotlin.String value=RunKt
            interceptorMapping: FUNCTION_REFERENCE 'update(InterceptorOperation): Unit' type=kotlin.reflect.KFunction1<@kotlin.ParameterName io.mockk.impl.interception.InterceptorOperation, kotlin.Unit> origin=null
              $this: GET_VAR 'this@IM: IM' type=IM origin=null

      FUN FAKE_OVERRIDE name:equals visibility:public modality:OPEN <> ($this:kotlin.Any, other:kotlin.Any?) returnType:Boolean flags:
        overridden:
          FUN IR_EXTERNAL_DECLARATION_STUB name:equals visibility:public modality:OPEN <> ($this:kotlin.Any, other:kotlin.Any?) returnType:Boolean flags:
        $this: VALUE_PARAMETER name:<this> type:kotlin.Any flags:
        VALUE_PARAMETER name:other index:0 type:kotlin.Any? flags:
      FUN FAKE_OVERRIDE name:hashCode visibility:public modality:OPEN <> ($this:kotlin.Any) returnType:Int flags:
        overridden:
          FUN IR_EXTERNAL_DECLARATION_STUB name:hashCode visibility:public modality:OPEN <> ($this:kotlin.Any) returnType:Int flags:
        $this: VALUE_PARAMETER name:<this> type:kotlin.Any flags:
      FUN FAKE_OVERRIDE name:toString visibility:public modality:OPEN <> ($this:kotlin.Any) returnType:String flags:
        overridden:
          FUN IR_EXTERNAL_DECLARATION_STUB name:toString visibility:public modality:OPEN <> ($this:kotlin.Any) returnType:String flags:
        $this: VALUE_PARAMETER name:<this> type:kotlin.Any flags:

    PROPERTY name:RunKtRegisterer type:kotlin.Unit visibility:private modality:FINAL flags:val

      FIELD PROPERTY_BACKING_FIELD name:RunKtRegisterer type:kotlin.Unit visibility:private
        EXPRESSION_BODY
          CALL 'register(): Unit' type=kotlin.Unit origin=null
            $this: GET_OBJECT 'IM' type=IM

      FUN DEFAULT_PROPERTY_ACCESSOR name:<get-RunKtRegisterer> visibility:private modality:FINAL <> () returnType:Unit flags:
        BLOCK_BODY
          RETURN type=kotlin.Nothing from='<get-RunKtRegisterer>(): Unit'
            GET_FIELD 'RunKtRegisterer: Unit' type=kotlin.Unit origin=null

    CLASS CLASS name:Test modality:FINAL visibility:public flags:
      $this: VALUE_PARAMETER INSTANCE_RECEIVER name:<this> type:Test flags:
      superClasses:
        CLASS IR_EXTERNAL_DECLARATION_STUB CLASS name:Any modality:OPEN visibility:public flags:
      CONSTRUCTOR visibility:public <> () returnType:Test flags:
        BLOCK_BODY
          DELEGATING_CONSTRUCTOR_CALL 'constructor Any()'
          INSTANCE_INITIALIZER_CALL classDescriptor='Test'

      FUN name:abc visibility:public modality:FINAL <> ($this:Test, a:kotlin.Int, b:kotlin.Int) returnType:Int flags:
        $this: VALUE_PARAMETER name:<this> type:Test flags:
        VALUE_PARAMETER name:a index:0 type:kotlin.Int flags:
        VALUE_PARAMETER name:b index:1 type:kotlin.Int flags:
        BLOCK_BODY
          RETURN type=kotlin.Nothing from='abc(Int, Int): Int'
            CALL 'intercept(vararg Any?, () -> Int): Int' type=kotlin.Int origin=null
              <T>: Int
              $this: CALL '<get-abc>(): Interceptor' type=io.mockk.impl.interception.Interceptor origin=GET_PROPERTY
                $this: GET_OBJECT 'IM' type=Test.IM
              args: VARARG type=Array<out Any?> varargElementType=Any?
                GET_VAR 'value-parameter a: Int' type=kotlin.Int origin=null
                GET_VAR 'value-parameter b: Int' type=kotlin.Int origin=null
              block: BLOCK type=() -> kotlin.Int origin=LAMBDA
                FUN LOCAL_FUNCTION_FOR_LAMBDA name:<anonymous> visibility:local modality:FINAL <> () returnType:Int flags:
                  BLOCK_BODY
                    RETURN type=kotlin.Nothing from='<anonymous>(): Int'
                      CALL 'plus(Int): Int' type=kotlin.Int origin=PLUS
                        $this: GET_VAR 'value-parameter a: Int' type=kotlin.Int origin=null
                        other: GET_VAR 'value-parameter b: Int' type=kotlin.Int origin=null
                FUNCTION_REFERENCE '<anonymous>(): Int' type=() -> kotlin.Int origin=LAMBDA

      CLASS OBJECT name:IM modality:FINAL visibility:public flags:
        $this: VALUE_PARAMETER INSTANCE_RECEIVER name:<this> type:Test.IM flags:
        superClasses:
          CLASS IR_EXTERNAL_DECLARATION_STUB CLASS name:Any modality:OPEN visibility:public flags:

        CONSTRUCTOR visibility:private <> () returnType:Test.IM flags:
          BLOCK_BODY
            DELEGATING_CONSTRUCTOR_CALL 'constructor Any()'
            INSTANCE_INITIALIZER_CALL classDescriptor='IM'

        PROPERTY name:abc type:io.mockk.impl.interception.Interceptor visibility:public modality:FINAL flags:var
          FIELD PROPERTY_BACKING_FIELD name:abc type:io.mockk.impl.interception.Interceptor visibility:public
            EXPRESSION_BODY
              GET_OBJECT 'PassThroughInterceptor' type=io.mockk.impl.interception.PassThroughInterceptor
          FUN DEFAULT_PROPERTY_ACCESSOR name:<get-abc> visibility:public modality:FINAL <> ($this:Test.IM) returnType:Interceptor flags:
            $this: VALUE_PARAMETER name:<this> type:Test.IM flags:
            BLOCK_BODY
              RETURN type=kotlin.Nothing from='<get-abc>(): Interceptor'
                GET_FIELD 'abc: Interceptor' type=io.mockk.impl.interception.Interceptor origin=null
                  receiver: GET_VAR 'this@IM: IM' type=Test.IM origin=null
          FUN DEFAULT_PROPERTY_ACCESSOR name:<set-abc> visibility:public modality:FINAL <> ($this:Test.IM, <set-?>:io.mockk.impl.interception.Interceptor) returnType:Unit flags:
            $this: VALUE_PARAMETER name:<this> type:Test.IM flags:
            VALUE_PARAMETER name:<set-?> index:0 type:io.mockk.impl.interception.Interceptor flags:
            BLOCK_BODY
              SET_FIELD 'abc: Interceptor' type=kotlin.Unit origin=null
                receiver: GET_VAR 'this@IM: IM' type=Test.IM origin=null
                value: GET_VAR 'value-parameter <set-?>: Interceptor' type=io.mockk.impl.interception.Interceptor origin=null

        FUN name:update visibility:public modality:FINAL <> ($this:Test.IM, op:io.mockk.impl.interception.InterceptorOperation) returnType:Unit flags:
          $this: VALUE_PARAMETER name:<this> type:Test.IM flags:
          VALUE_PARAMETER name:op index:0 type:io.mockk.impl.interception.InterceptorOperation flags:
          BLOCK_BODY
            CALL '<set-abc>(Interceptor): Unit' type=kotlin.Unit origin=EQ
              $this: GET_VAR 'this@IM: IM' type=Test.IM origin=null
              <set-?>: CALL 'applyOperation(Interceptor, String, vararg KClass<*>): Interceptor' type=io.mockk.impl.interception.Interceptor origin=null
                $this: GET_VAR 'value-parameter op: InterceptorOperation' type=io.mockk.impl.interception.InterceptorOperation origin=null
                interceptor: CALL '<get-abc>(): Interceptor' type=io.mockk.impl.interception.Interceptor origin=GET_PROPERTY
                  $this: GET_VAR 'this@IM: IM' type=Test.IM origin=null
                name: CONST String type=kotlin.String value=abc
                types: VARARG type=Array<out KClass<*>> varargElementType=KClass<*>
                  CLASS_REFERENCE 'Int' type=kotlin.reflect.KClass<kotlin.Int>
                  CLASS_REFERENCE 'Int' type=kotlin.reflect.KClass<kotlin.Int>

        FUN name:register visibility:public modality:FINAL <> ($this:Test.IM) returnType:Unit flags:
          $this: VALUE_PARAMETER name:<this> type:Test.IM flags:
          BLOCK_BODY
            CALL 'register(String, (InterceptorOperation) -> Unit): Unit' type=kotlin.Unit origin=null
              $this: GET_OBJECT 'Interceptors' type=io.mockk.impl.interception.Interceptors
              className: BLOCK type=kotlin.String origin=EXCLEXCL
                VAR IR_TEMPORARY_VARIABLE name:tmp0_notnull type:kotlin.String? flags:val
                  CALL '<get-qualifiedName>(): String?' type=kotlin.String? origin=GET_PROPERTY
                    $this: CLASS_REFERENCE 'Test' type=kotlin.reflect.KClass<Test>
                WHEN type=kotlin.String origin=null
                  BRANCH
                    if: CALL 'EQEQ(Any?, Any?): Boolean' type=kotlin.Boolean origin=EQEQ
                      arg0: GET_VAR 'tmp0_notnull: String?' type=kotlin.String? origin=null
                      arg1: CONST Null type=kotlin.Nothing? value=null
                    then: CALL 'THROW_NPE(): Nothing' type=kotlin.Nothing origin=EXCLEXCL
                  BRANCH
                    if: CONST Boolean type=kotlin.Boolean value=true
                    then: GET_VAR 'tmp0_notnull: String?' type=kotlin.String? origin=null
              interceptorMapping: FUNCTION_REFERENCE 'update(InterceptorOperation): Unit' type=kotlin.reflect.KFunction1<@kotlin.ParameterName io.mockk.impl.interception.InterceptorOperation, kotlin.Unit> origin=null
                $this: GET_VAR 'this@IM: IM' type=Test.IM origin=null

        FUN FAKE_OVERRIDE name:equals visibility:public modality:OPEN <> ($this:kotlin.Any, other:kotlin.Any?) returnType:Boolean flags:
          overridden:
            FUN IR_EXTERNAL_DECLARATION_STUB name:equals visibility:public modality:OPEN <> ($this:kotlin.Any, other:kotlin.Any?) returnType:Boolean flags:
          $this: VALUE_PARAMETER name:<this> type:kotlin.Any flags:
          VALUE_PARAMETER name:other index:0 type:kotlin.Any? flags:

        FUN FAKE_OVERRIDE name:hashCode visibility:public modality:OPEN <> ($this:kotlin.Any) returnType:Int flags:
          overridden:
            FUN IR_EXTERNAL_DECLARATION_STUB name:hashCode visibility:public modality:OPEN <> ($this:kotlin.Any) returnType:Int flags:
          $this: VALUE_PARAMETER name:<this> type:kotlin.Any flags:

        FUN FAKE_OVERRIDE name:toString visibility:public modality:OPEN <> ($this:kotlin.Any) returnType:String flags:
          overridden:
            FUN IR_EXTERNAL_DECLARATION_STUB name:toString visibility:public modality:OPEN <> ($this:kotlin.Any) returnType:String flags:
          $this: VALUE_PARAMETER name:<this> type:kotlin.Any flags:

      FUN FAKE_OVERRIDE name:equals visibility:public modality:OPEN <> ($this:kotlin.Any, other:kotlin.Any?) returnType:Boolean flags:
        overridden:
          FUN IR_EXTERNAL_DECLARATION_STUB name:equals visibility:public modality:OPEN <> ($this:kotlin.Any, other:kotlin.Any?) returnType:Boolean flags:
        $this: VALUE_PARAMETER name:<this> type:kotlin.Any flags:
        VALUE_PARAMETER name:other index:0 type:kotlin.Any? flags:

      FUN FAKE_OVERRIDE name:hashCode visibility:public modality:OPEN <> ($this:kotlin.Any) returnType:Int flags:
        overridden:
          FUN IR_EXTERNAL_DECLARATION_STUB name:hashCode visibility:public modality:OPEN <> ($this:kotlin.Any) returnType:Int flags:
        $this: VALUE_PARAMETER name:<this> type:kotlin.Any flags:

      FUN FAKE_OVERRIDE name:toString visibility:public modality:OPEN <> ($this:kotlin.Any) returnType:String flags:
        overridden:
          FUN IR_EXTERNAL_DECLARATION_STUB name:toString visibility:public modality:OPEN <> ($this:kotlin.Any) returnType:String flags:
        $this: VALUE_PARAMETER name:<this> type:kotlin.Any flags:

    PROPERTY name:TestRegisterer type:kotlin.Unit visibility:private modality:FINAL flags:val

      FIELD PROPERTY_BACKING_FIELD name:TestRegisterer type:kotlin.Unit visibility:private
        EXPRESSION_BODY
          CALL 'register(): Unit' type=kotlin.Unit origin=null
            $this: GET_OBJECT 'IM' type=Test.IM

      FUN DEFAULT_PROPERTY_ACCESSOR name:<get-TestRegisterer> visibility:private modality:FINAL <> () returnType:Unit flags:
        BLOCK_BODY
          RETURN type=kotlin.Nothing from='<get-TestRegisterer>(): Unit'
            GET_FIELD 'TestRegisterer: Unit' type=kotlin.Unit origin=null


 */
