<template>
  <common-drawer
    ref="dialogRef"
    title="条板转换办理"
    v-model="dialogVisible"
    :size="step===0 ? '80%' : '960'"
    :before-close="handleClose"
    append-to-body
    :close-on-click-modal="false"
     :wrapper-closable="false"
    custom-class="wms-outbound-handling"
  >
    <template #titleRight>
      <span class="step-btn">
        <common-button size="mini" plain :disabled="step === 0" @click="preStep">上一步</common-button>
        <common-button size="mini" plain :disabled="step === stepOptions.length - 1" @click="handleNextStep">下一步</common-button>
        <common-button :loading="submitLoading" size="mini" type="warning" :disabled="step !== stepOptions.length - 1" @click="submit">
          确认提交
        </common-button>
      </span>
    </template>
    <template #content>
      <template v-if="dialogVisible">
        <div v-show="step === 0">
          <steelCoilPlateForm ref="plateRef" :visibleVal="dialogVisible" :basic-class="props.basicClass" :material="props.material" :max-height="maxHeight" :form-data="form.plateObj"/>
        </div>
        <div v-show="step === 1">
          <steelCoilPreview ref="previewRef" :visibleVal="dialogVisible" :basic-class="props.basicClass" :material="props.material" :max-height="maxHeight" :form-data="form.outbound"/>
        </div>
      </template>
    </template>
  </common-drawer>
</template>

<script setup>
import { steelCoilConvertHandling } from '@/api/wms/material-outbound/raw-material/outbound-handling'
import { defineEmits, defineProps, provide, ref, reactive, watch, nextTick } from 'vue'

import { outboundDestinationTypeEnum } from '@/utils/enum/modules/wms'
import { convertUnits } from '@/utils/convert/unit'

import { isNotBlank } from '@data-type/index'
import { numFmtByUnit, numFmtByBasicClass } from '@/utils/wms/convert-unit'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import steelCoilPlateForm from './steel-coil-plate-form.vue'
import steelCoilPreview from './steel-coil-preview.vue'
import useWmsConfig from '@/composables/store/use-wms-config'
import { ElMessage } from 'element-plus'

const emit = defineEmits(['success', 'update:visible'])

const props = defineProps({
  visible: {
    type: Boolean,
    require: true
  },
  basicClass: {
    // 基础分类
    type: Number
  },
  material: {
    // 物料信息
    type: Object
  }
})

const plateRef = ref()
const previewRef = ref()
const form = ref({
  plateObj: {
    list: []
  },
  outbound: {
    plateForm: {
      totalObj: {}
    },
    surplusForm: {
      totalObj: {}
    }
  }
})
// const submitLoading = ref(false)
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })
const { outboundCfg } = useWmsConfig()
provide('outboundCfg', outboundCfg)

const dialogRef = ref()
const stepOptions = reactive([{ title: '条板处理' }, { title: '预览' }])
const step = ref(0)
const submitLoading = ref(false)

function nextStepValidate() {
  switch (step.value) {
    case 0:
      return plateRef.value.validateSubmit()
    case 1:
      return previewRef.value.validateSubmit()
  }
}

watch(
  () => dialogVisible.value,
  (val) => {
    if (val) {
      step.value = 0
      form.value = {
        plateObj: {
          list: []
        },
        outbound: {
          plateForm: {
            totalObj: {}
          },
          surplusForm: {
            totalObj: {}
          }
        }
      }
    }
  }
)

// 表格最大高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.wms-outbound-handling',
    extraBox: [
      '.el-dialog__header',
      '.plate-out-material-info',
      '.material-outbound-mode-info',
      '.form-info',
      '.other-info'
    ],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false,
    minHeight: 300,
    extraHeight: 200
  },
  dialogVisible
)

function preStep() {
  if (step.value === 1) {
    previewRef.value && previewRef.value.assignForm()
  }
  step.value--
}

async function handleNextStep() {
  const valid = await nextStepValidate()
  if (!valid) {
    return
  }
  if (step.value === 0) {
    const list = JSON.parse(JSON.stringify(form.value.plateObj.list)) || []
    const formVal = JSON.parse(JSON.stringify(form.value.plateObj))
    const boolSurplusTrueList = []
    const boolSurplusFalseList = []
    list.forEach(v => {
      if (v.boolSurplus) {
        boolSurplusTrueList.push(v)
      } else {
        boolSurplusFalseList.push(v)
      }
    })
    form.value.outbound.plateForm.totalObj = {}
    form.value.outbound.surplusForm.totalObj = {}
    const widthArr = []
    form.value.outbound.plateForm.totalObj.segmentQuantity = 0
    form.value.outbound.plateForm.totalObj.quantity = 0
    form.value.outbound.plateForm.totalObj.width = 0
    form.value.outbound.plateForm.totalObj.mete = 0
    boolSurplusFalseList.forEach(v => {
      form.value.outbound.plateForm.totalObj.specification = props.material.specification
      form.value.outbound.plateForm.totalObj.segmentQuantity++
      form.value.outbound.plateForm.totalObj.thickness = v.thickness
      form.value.outbound.plateForm.totalObj.quantity += v.quantity
      form.value.outbound.plateForm.totalObj.width += (v.width * v.quantity)
      form.value.outbound.plateForm.totalObj.mete += v.mete
      if (!widthArr.includes(v.width)) {
        widthArr.push(v.width)
      }
    })
    form.value.outbound.plateForm.boolSurplus = false
    form.value.outbound.plateForm.boolOutbound = isNotBlank(form.value.outbound.plateForm.boolOutbound) ? form.value.outbound.plateForm.boolOutbound : true
    form.value.outbound.plateForm.totalObj.segmentQuantity = form.value.outbound.plateForm.totalObj.segmentQuantity / widthArr.length
    form.value.outbound.plateForm.totalObj.width = form.value.outbound.plateForm.totalObj.width / form.value.outbound.plateForm.totalObj.segmentQuantity
    form.value.outbound.plateForm.basicClass = props.basicClass
    form.value.outbound.plateForm.factoryId = props.material?.factory?.id
    form.value.outbound.plateForm.projectId = form.value.outbound.plateForm.projectId || props.material?.project?.id || (form.value.outbound.plateForm?.boolOutbound ? undefined : 'common')
    form.value.outbound.plateForm.monomerId = (form.value.outbound.plateForm.projectId && form.value.outbound.plateForm.projectId !== 'common') ? form.value.outbound.plateForm.monomerId || (form.value.outbound.plateForm.projectId === props.material?.project?.id ? (props.material?.monomer?.id || props.material?.monomerId) : undefined) : undefined
    form.value.outbound.plateForm.areaId = form.value.outbound.plateForm.monomerId ? (form.value.outbound.plateForm.areaId || (form.value.outbound.plateForm.monomerId === (props.material?.monomer?.id || props.material?.monomerId) ? (props.material?.area?.id || props.material?.areaId) : undefined)) : undefined
    form.value.outbound.plateForm.recipientId = form.value.outbound.plateForm.recipientId || formVal.recipientId
    form.value.outbound.plateForm.outboundAddress = form.value.outbound.plateForm.outboundAddress || (form.value.outbound.plateForm.boolOutbound ? outboundDestinationTypeEnum.FACTORY.V : undefined)
    form.value.outbound.surplusForm.totalObj.segmentQuantity = 0
    form.value.outbound.surplusForm.totalObj.quantity = 0
    form.value.outbound.surplusForm.totalObj.width = 0
    form.value.outbound.surplusForm.totalObj.mete = 0
    boolSurplusTrueList.forEach(v => {
      form.value.outbound.surplusForm.totalObj.specification = props.material.specification
      // form.value.outbound.surplusForm.totalObj.segmentQuantity++
      form.value.outbound.surplusForm.totalObj.thickness = v.thickness
      form.value.outbound.surplusForm.totalObj.quantity += v.quantity
      form.value.outbound.surplusForm.totalObj.width += v.width
      form.value.outbound.surplusForm.totalObj.mete += v.mete
    })
    form.value.outbound.surplusForm.totalObj.width = boolSurplusTrueList.length ? form.value.outbound.surplusForm.totalObj.width / boolSurplusTrueList.length : 0
    form.value.outbound.surplusForm.totalObj.segmentQuantity = form.value.outbound.surplusForm.totalObj.quantity
    form.value.outbound.surplusForm.boolSurplus = true
    form.value.outbound.surplusForm.boolOutbound = isNotBlank(form.value.outbound.surplusForm.boolOutbound) ? form.value.outbound.surplusForm.boolOutbound : false
    form.value.outbound.surplusForm.basicClass = props.basicClass
    form.value.outbound.surplusForm.basicClass = props.basicClass
    form.value.outbound.surplusForm.factoryId = props.material?.factory?.id
    form.value.outbound.surplusForm.projectId = form.value.outbound.surplusForm.projectId || props.material?.project?.id || (form.value.outbound.surplusForm?.boolOutbound ? undefined : 'common')
    form.value.outbound.surplusForm.monomerId = (form.value.outbound.surplusForm.projectId && form.value.outbound.surplusForm.projectId !== 'common') ? form.value.outbound.surplusForm.monomerId || (form.value.outbound.surplusForm.projectId === props.material?.project?.id ? (props.material?.monomer?.id || props.material?.monomerId) : undefined) : undefined
    form.value.outbound.surplusForm.areaId = form.value.outbound.surplusForm.monomerId ? (form.value.outbound.surplusForm.areaId || (form.value.outbound.surplusForm.monomerId === (props.material?.monomer?.id || props.material?.monomerId) ? (props.material?.area?.id || props.material?.areaId) : undefined)) : undefined
    form.value.outbound.surplusForm.recipientId = form.value.outbound.surplusForm.recipientId || formVal.recipientId
    form.value.outbound.surplusForm.outboundAddress = form.value.outbound.surplusForm.outboundAddress || (form.value.outbound.surplusForm.boolOutbound ? outboundDestinationTypeEnum.FACTORY.V : undefined)
  }
  console.log(form.value.outbound)
  step.value++
}

// // 重置表单
// function resetForm() {
//   previewRef.value && previewRef.value.reset()
// }

// // 清空校验
function clearValidate() {
  previewRef.value && previewRef.value.clearValidate()
  plateRef.value && plateRef.value.clearValidate()
}

// 显示钩子
function showHook() {
  nextTick(() => {
    clearValidate()
  })
}

// 表单提交
async function submit() {
  const valid = await previewRef.value.validateSubmit()
  if (!valid) {
    return
  }
  const submitData = JSON.parse(JSON.stringify(form.value.plateObj))
  submitData.list.forEach(v => {
    if (!v.boolSurplus) {
      v.boolOutbound = form.value.outbound.plateForm.boolOutbound
      v.projectId = form.value.outbound.plateForm.projectId === 'common' ? null : form.value.outbound.plateForm.projectId
      v.monomerId = form.value.outbound.plateForm.monomerId
      v.areaId = form.value.outbound.plateForm.areaId
      v.workshopId = v.boolOutbound ? form.value.outbound.plateForm.workshopId : undefined
      v.recipientId = v.boolOutbound ? form.value.outbound.plateForm.recipientId : undefined
      v.outboundTime = v.boolOutbound ? form.value.outbound.plateForm.outboundTime : undefined
      v.warehouseId = form.value.outbound.plateForm?.warehouseId
      v.outboundAddress = v.boolOutbound ? form.value.outbound.plateForm.outboundAddress : undefined
    } else {
      v.boolOutbound = form.value.outbound.surplusForm?.boolOutbound
      v.projectId = form.value.outbound.surplusForm.projectId === 'common' ? null : form.value.outbound.surplusForm.projectId
      v.monomerId = form.value.outbound.surplusForm.monomerId
      v.areaId = form.value.outbound.surplusForm.areaId
      v.workshopId = v.boolOutbound ? form.value.outbound.surplusForm.workshopId : undefined
      v.recipientId = v.boolOutbound ? form.value.outbound.surplusForm.recipientId : undefined
      v.outboundTime = v.boolOutbound ? form.value.outbound.surplusForm.outboundTime : undefined
      v.warehouseId = form.value.outbound.surplusForm?.warehouseId
      v.outboundAddress = v.boolOutbound ? form.value.outbound.surplusForm.outboundAddress : undefined
    }
  })
  submitData.battenList = submitData.list
  submitData.battenList = await numFmtByBasicClass(submitData.battenList, { toSmallest: true, toNum: true }, { weight: ['mete'] })
  submitData.quantity = convertUnits(submitData.quantity, 'mm', 'm')
  await numFmtByUnit(submitData, {
    unit: submitData.outboundUnit,
    precision: submitData.outboundUnitPrecision,
    fields: ['quantity', 'totalWeight'],
    toSmallest: true,
    toNum: true
  })
  try {
    submitLoading.value = true
    await steelCoilConvertHandling(submitData)
    ElMessage.success('条板转换已提交')
    emit('success')
    handleClose()
  } catch (error) {
    console.log('条板转换办理', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
