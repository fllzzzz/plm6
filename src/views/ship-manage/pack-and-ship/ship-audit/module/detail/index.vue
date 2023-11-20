<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    v-model="dialogVisible"
    :title="title"
    top="5vh"
    width="65vw"
    custom-class="audit-detail"
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
        <el-descriptions-item label-class-name="contractLabel" label="本次发货额">
          {{ toFixed(contract.deliveryAmount, decimalPrecision.shipment) }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="安全余额">
          {{ toFixed(contract.safeAmount, decimalPrecision.shipment) }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="合同额">
          {{ toFixed(contract.contractAmount, decimalPrecision.shipment) }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="累计收款">
          {{ toFixed(contract.totalCollectionAmount, decimalPrecision.shipment) }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="累计发运">
          {{ toFixed(contract.totalDeliveryAmount, decimalPrecision.shipment) }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="累计发运额">
          {{ toFixed(contract.totalDeliveryAmount, decimalPrecision.shipment) }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="合同应收">
          {{ toFixed(contract.contractReceivableAmount, decimalPrecision.shipment) }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="开票应收">
          {{ toFixed(contract.billingReceivableAmount, decimalPrecision.shipment) }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="附件">
          <div class="imgs-box">
            <el-image
              v-for="url in contract.attachmentDTOS"
              :preview-src-list="contract.attachmentImgSrc"
              :initial-index="1"
              :key="url.id"
              :src="url.tinyImageUrl"
              lazy
              style="margin:0 2px;"
            ></el-image>
          </div>
        </el-descriptions-item>
      </el-descriptions>
      <el-radio-group v-model="curProductType" v-if="productTypeBits.length > 1" size="small" class="filter-item">
        <el-radio-button
          v-if="packTypeEnum.STRUCTURE.V & productType"
          :label="packTypeEnum.STRUCTURE.V"
          :disabled="artifactList.length == 0"
          >{{ packTypeEnum.STRUCTURE.L }}({{ artifactList.length }})</el-radio-button
        >
        <el-radio-button
          v-if="packTypeEnum.MACHINE_PART.V & productType"
          :label="packTypeEnum.MACHINE_PART.V"
          :disabled="partList.length == 0"
          >{{ packTypeEnum.MACHINE_PART.L }}({{ partList.length }})</el-radio-button
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
          :disabled="auxiliaryMaterialList.length == 0"
          >{{ packTypeEnum.AUXILIARY_MATERIAL.L }}({{ auxiliaryMaterialList.length }})</el-radio-button
        >
      </el-radio-group>
    </div>
    <component
      :is="currentView"
      :measureUnit="contract?.enclosureMeasureMode === enclosureSettlementTypeEnum.AREA.V? '㎡' : 'm'"
      v-loading="tableLoading"
      :maxHeight="maxHeight"
      :list="list"
    />
  </common-dialog>
</template>

<script setup>
import { defineProps, ref, defineEmits, watch, computed } from 'vue'
import { ElRadioGroup } from 'element-plus'

import { packTypeEnum } from '@enum-ms/mes'
import { weightMeasurementModeEnum, enclosureSettlementTypeEnum } from '@enum-ms/finance'
import { pricingMannerEnum } from '@enum-ms/contract'
import { weightTypeEnum } from '@enum-ms/common'
import { convertUnits } from '@/utils/convert/unit'
import { DP } from '@/settings/config'
import { toFixed } from '@/utils/data-type'
import EO from '@enum'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import structureTable from './module/structure'
import partTable from './module/part'
import enclosureTable from './module/enclosure'
import auxiliaryMaterialTable from './module/auxiliary-material'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

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
    mainBox: '.audit-detail',
    extraBox: ['.el-dialog__header', '.head-container'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false
  },
  dialogVisible
)

const tableLoading = ref(false)
const artifactList = ref([])
const partList = ref([])
const enclosureList = ref([])
const auxiliaryMaterialList = ref([])
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
    case packTypeEnum.MACHINE_PART.V:
      return partTable
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
          v.totalLength = convertUnits(v.length * v.showQuantity || 0, 'mm', 'm')
          v.totalMete =
            contract.value.structureMeasureMode === weightMeasurementModeEnum.OVERWEIGHT.V
              ? convertUnits(v.totalWeight, 'kg', 't')
              : convertUnits(v.weight * v.showQuantity, 'kg', 't')
          v.totalPrice = v.pricingManner === pricingMannerEnum.WEIGHT.V ? v.totalMete * (v.unitPrice || 0) : v.totalLength * (v.unitPrice || 0)
          return v
        })
      )
    case packTypeEnum.MACHINE_PART.V:
      return (
        partList.value &&
        partList.value.map((v) => {
          v.showQuantity = v[props.quantityFelid]
          v.weight = (props.weightType === weightTypeEnum.NET.V ? v.netWeight : v.grossWeight) || 0
          v.totalLength = convertUnits(v.length * v.showQuantity || 0, 'mm', 'm')
          v.totalMete =
            contract.value.structureMeasureMode === weightMeasurementModeEnum.OVERWEIGHT.V
              ? convertUnits(v.totalWeight, 'kg', 't')
              : convertUnits(v.weight * v.showQuantity, 'kg', 't')
          v.totalPrice = v.pricingManner === pricingMannerEnum.WEIGHT.V ? v.totalMete * (v.unitPrice || 0) : v.totalLength * (v.unitPrice || 0)
          return v
        })
      )
    case packTypeEnum.ENCLOSURE.V:
      return (
        enclosureList.value &&
        enclosureList.value.map((v) => {
          v.showQuantity = v[props.quantityFelid]
          v.totalMete =
            v.pricingManner === enclosureSettlementTypeEnum.AREA.V
              ? toFixed(v.totalArea, DP.COM_ENCLOSURE_AREA__M2)
              : convertUnits(v.totalLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M)
          v.totalPrice = v.unitPrice * v.totalMete || 0
          return v
        })
      )
    case packTypeEnum.AUXILIARY_MATERIAL.V:
      return (
        auxiliaryMaterialList.value &&
        auxiliaryMaterialList.value.map((v) => {
          v.showQuantity = v[props.quantityFelid]
          v.fullClassName = `${v.firstName}/${v.secondName}/${v.thirdName}`
          v.totalPrice = v.unitPrice * v.shipMete || 0
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
  partList.value = []
  enclosureList.value = []
  auxiliaryMaterialList.value = []
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
    partList.value = data.partList || []
    enclosureList.value = data.enclosureList || []
    auxiliaryMaterialList.value = data.auxiliaryMaterialList || []
    contract.value = data.review || {}
    contract.value.attachmentImgSrc = contract.value.attachmentDTOS && contract.value.attachmentDTOS.map((k) => k.imageUrl)
  } catch (error) {
    console.log('详情', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
<style lang="scss" scoped>
.imgs-box {
  & > .el-image {
    width: 50px;
    height: 40px;
    border: 2px solid #dcdfe6;
    border-radius: 6px;
    background-color: white;
    cursor: pointer;
    + .el-image {
      margin-left: -40px;
    }
  }
}
</style>
