<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    v-model="dialogVisible"
    :title="title"
    top="5vh"
    width="65vw"
    @closed="handleClose"
  >
    <template #titleAfter> <slot name="tip"></slot> </template>
    <template #titleRight>
      <slot name="titleRight" />
    </template>
    <div class="head-container">
      <el-descriptions :column="2" border style="margin-bottom: 10px" v-loading="tableLoading" v-if="contract">
        <el-descriptions-item label-class-name="contractLabel" label="项目名称">{{ contract.name }}</el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="车牌号">{{ contract.licensePlate }}</el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="本次发货额">{{
          toFixed(contract.deliveryAmount, DP.YUAN)
        }}</el-descriptions-item>
        <el-descriptions-item
label-class-name="contractLabel"
label="安全余额"
          >{{ toFixed(contract.safeAmount, DP.YUAN) }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="合同额">{{
          toFixed(contract.contractAmount, DP.YUAN)
        }}</el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="累计收款">{{
          toFixed(contract.totalCollectionAmount, DP.YUAN)
        }}</el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="累计发运">{{
          toFixed(contract.totalDeliveryAmount, DP.YUAN)
        }}</el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="累计发运额">{{
          toFixed(contract.totalDeliveryAmount, DP.YUAN)
        }}</el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="合同应收">{{
          toFixed(contract.contractReceivableAmount, DP.YUAN)
        }}</el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="开票应收">{{
          toFixed(contract.billingReceivableAmount, DP.YUAN)
        }}</el-descriptions-item>
      </el-descriptions>
      <el-radio-group v-model="curProductType" v-if="productTypeBits.length > 1" size="small" class="filter-item">
        <el-radio-button
          v-if="packTypeEnum.STRUCTURE.V & productType"
          :label="packTypeEnum.STRUCTURE.V"
          :disabled="artifactList.length == 0"
          >{{ packTypeEnum.STRUCTURE.L }}({{ artifactList.length }})</el-radio-button
        >
        <el-radio-button
          v-if="packTypeEnum.ENCLOSURE.V & productType"
          :label="packTypeEnum.ENCLOSURE.V"
          :disabled="enclosureList.length == 0"
          >{{ packTypeEnum.ENCLOSURE.L }}({{ enclosureList.length }})</el-radio-button
        >
        <el-radio-button
          v-if="packTypeEnum.AUXILIARY_MATERIAL.V & productType"
          :label="packTypeEnum.AUXILIARY_MATERIAL.V"
          :disabled="auxList.length == 0"
          >{{ packTypeEnum.AUXILIARY_MATERIAL.L }}({{ auxList.length }})</el-radio-button
        >
      </el-radio-group>
    </div>
    <component
      :is="currentView"
      :measureUnit="contract?.enclosureMeasureMode === 2 ? '㎡' : 'm'"
      v-loading="tableLoading"
      :maxHeight="maxHeight"
      :list="list"
    />
  </common-dialog>
</template>

<script setup>
// TODO:MeasureModeENUM
import { defineProps, ref, defineEmits, watch, computed } from 'vue'
import { ElRadioGroup } from 'element-plus'

import { packTypeEnum } from '@enum-ms/mes'
import { weightTypeEnum } from '@enum-ms/common'
import { convertUnits } from '@/utils/convert/unit'
import { DP } from '@/settings/config'
import { toFixed } from '@/utils/data-type'
import EO from '@enum'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import structureTable from './module/structure'
import enclosureTable from './module/enclosure'
import auxiliaryMaterialTable from './module/auxiliary-material'

const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  title: {
    type: String,
    default: ''
  },
  detailInfo: {
    type: Object,
    required: true
  },
  weightType: {
    type: [Number, String],
    default: weightTypeEnum.NET.V
  },
  detailFunc: {
    type: Function,
    required: true
  },
  quantityFelid: {
    type: String,
    default: 'shipQuantity'
  }
})
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-dialog__header', '.head-container'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true
  },
  dialogVisible
)

const tableLoading = ref(false)
const artifactList = ref([])
const enclosureList = ref([])
const auxList = ref([])
const contract = ref({})
const curProductType = ref()

const productType = computed(() => {
  return props.detailInfo && props.detailInfo.productType
})
const productTypeBits = computed(() => {
  return EO.getBits(packTypeEnum, productType.value, 'V')
})
const detailId = computed(() => {
  return (props.detailInfo && props.detailInfo.id) || undefined
})
const currentView = computed(() => {
  switch (curProductType.value) {
    case packTypeEnum.STRUCTURE.V:
      return structureTable
    case packTypeEnum.ENCLOSURE.V:
      return enclosureTable
    case packTypeEnum.AUXILIARY_MATERIAL.V:
      return auxiliaryMaterialTable
    default:
      return ''
  }
})
const list = computed(() => {
  switch (curProductType.value) {
    case packTypeEnum.STRUCTURE.V:
      return (
        artifactList.value &&
        artifactList.value.map((v) => {
          v.showQuantity = v[props.quantityFelid]
          v.weight = (props.weightType === weightTypeEnum.NET.V ? v.netWeight : v.grossWeight) || 0
          v.totalMete =
            contract.value.structureMeasureMode === 2
              ? convertUnits(v.totalWeight, 'kg', 't')
              : convertUnits(v.weight * v.showQuantity, 'kg', 't')
          v.totalPrice = v.unitPrice * v.totalMete || 0
          return v
        })
      )
    case packTypeEnum.ENCLOSURE.V:
      return (
        enclosureList.value &&
        enclosureList.value.map((v) => {
          v.showQuantity = v[props.quantityFelid]
          v.totalMete =
            contract.value.enclosureMeasureMode === 2
              ? toFixed(v.totalArea, DP.COM_AREA__M2)
              : convertUnits(v.totalLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M)
          v.totalPrice = v.unitPrice * v.totalMete || 0
          return v
        })
      )
    case packTypeEnum.AUXILIARY_MATERIAL.V:
      return (
        auxList.value &&
        auxList.value.map((v) => {
          v.showQuantity = v[props.quantityFelid]
          v.fullClassName = `${v.firstName}/${v.secondName}/${v.thirdName}`
          v.totalPrice = v.unitPrice * v.showQuantity || 0
          return v
        })
      )
    default:
      return []
  }
})

watch(
  () => detailId.value,
  (val) => {
    if (val) {
      fetchDetail()
    }
  }
)

function init() {
  artifactList.value = []
  enclosureList.value = []
  auxList.value = []
  contract.value = {}
  curProductType.value = undefined
}

async function fetchDetail() {
  try {
    init()
    tableLoading.value = true
    curProductType.value = productTypeBits.value[0]
    const data = await props.detailFunc(detailId.value)
    artifactList.value = data.artifactList || []
    enclosureList.value = data.enclosureList || []
    auxList.value = data.auxList || []
    contract.value = data.review || {}
  } catch (error) {
    console.log('详情', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
