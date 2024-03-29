<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    v-model="dialogVisible"
    :title="title"
    top="5vh"
    width="85vw"
    @closed="handleClose"
  >
    <template #titleAfter> <slot name="tip"></slot> </template>
    <template #titleRight>
      <slot name="titleRight" />
    </template>
    <div class="head-container">
      <div style="color:red;margin-bottom:8px;" v-if="(detailInfo.deliveryStatus===deliveryStatusEnum.RETURN.V || detailInfo.shipmentStatus===deliveryReceiptStatusEnum.RETURN.V) && detailInfo.cancelDeliveryReason">当前发运已取消，取消原因：{{detailInfo.cancelDeliveryReason}}</div>
      <el-radio-group v-model="curProductType" v-if="productTypeBits.length > 1" size="small" class="filter-item">
        <el-radio-button
          v-if="packTypeEnum.BOX.V & productType"
          :label="packTypeEnum.BOX.V"
          :disabled="boxList.length == 0"
          >{{ packTypeEnum.BOX.L }}({{ boxList.length }})</el-radio-button
        >
        <el-radio-button
          v-if="packTypeEnum.MACHINE_PART.V & productType"
          :label="packTypeEnum.MACHINE_PART.V"
          :disabled="partList.length == 0"
          >{{ packTypeEnum.MACHINE_PART.L }}({{ partList.length }})</el-radio-button
        >
        <el-radio-button
          v-if="packTypeEnum.AUXILIARY_MATERIAL.V & productType"
          :label="packTypeEnum.AUXILIARY_MATERIAL.V"
          :disabled="auxiliaryMaterialList.length == 0"
          >{{ packTypeEnum.AUXILIARY_MATERIAL.L }}({{ auxiliaryMaterialList.length }})</el-radio-button
        >
      </el-radio-group>
      <common-radio-button
        v-model="monomerStatus"
        :options="SummaryStatusEnum"
        type="enum"
        size="small"
        class="filter-item"
      />
    </div>
    <component
      :is="currentView"
      v-loading="tableLoading"
      :maxHeight="maxHeight"
      :list="list"
      :isSuspend="monomerStatus === SummaryStatusEnum.SUSPEND.V"
    />
  </common-dialog>
</template>

<script setup>
import { defineProps, ref, defineEmits, watch, computed } from 'vue'
import { ElRadioGroup } from 'element-plus'

import { deliveryStatusEnum, deliveryReceiptStatusEnum } from '@enum-ms/mes'
import { bridgePackTypeEnum as packTypeEnum } from '@enum-ms/bridge'
import { weightTypeEnum } from '@enum-ms/common'
import { convertUnits } from '@/utils/convert/unit'
import EO from '@enum'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import structureTable from './module/structure'
import enclosureTable from './module/enclosure'
import auxiliaryMaterialTable from './module/auxiliary-material'

const SummaryStatusEnum = {
  PROCESS: { L: '单体汇总', K: 'PROCESS', V: 0 },
  SUSPEND: { L: '区域汇总', K: 'SUSPEND', V: 1 }
}

const emit = defineEmits(['update:visible', 'getDetail'])
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
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    extraHeight: 150
  },
  dialogVisible
)

const monomerStatus = ref(SummaryStatusEnum.PROCESS.V)
const tableLoading = ref(false)
const boxList = ref([])
const partList = ref([])
const auxiliaryMaterialList = ref([])
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
    case packTypeEnum.BOX.V:
      return structureTable
    case packTypeEnum.MACHINE_PART.V:
      return enclosureTable
    case packTypeEnum.AUXILIARY_MATERIAL.V:
      return auxiliaryMaterialTable
    default:
      return ''
  }
})
const list = computed(() => {
  switch (curProductType.value) {
    case packTypeEnum.BOX.V:
      return (
        boxList.value &&
        boxList.value.map((v) => {
          v.showQuantity = v[props.quantityFelid]
          v.weight = (props.weightType === weightTypeEnum.NET.V ? v.netWeight : v.grossWeight) || 0
          v.totalWeight = convertUnits(v.weight * v.showQuantity, 'kg', 't')
          return v
        })
      )
    case packTypeEnum.MACHINE_PART.V:
      return (
        partList.value &&
        partList.value.map((v) => {
          v.showQuantity = v[props.quantityFelid]
          v.totalLength = convertUnits(v.length * v.showQuantity, 'mm', 'm')
          v.totalWeight = convertUnits(v.totalWeight, 'kg', 't')
          return v
        })
      )
    case packTypeEnum.AUXILIARY_MATERIAL.V:
      return (
        auxiliaryMaterialList.value &&
        auxiliaryMaterialList.value.map((v) => {
          v.showQuantity = v[props.quantityFelid]
          v.fullClassName = `${v.firstName}/${v.secondName}/${v.thirdName}`
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

async function fetchDetail() {
  try {
    tableLoading.value = true
    curProductType.value = productTypeBits.value[0]
    const data = await props.detailFunc(detailId.value)
    emit('getDetail', detailId.value, data)
    boxList.value = data.boxList || []
    partList.value = data.partList || []
    auxiliaryMaterialList.value = data.auxiliaryMaterialList || []
  } catch (error) {
    console.log('详情', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
