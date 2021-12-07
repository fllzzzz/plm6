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
      <slot name="contract" :contract="contract"></slot>
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
      <common-radio-button
        v-if="!isShowPrice"
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
      :isShowPrice="isShowPrice"
      :isSuspend="monomerStatus === SummaryStatusEnum.SUSPEND.V"
    />
  </common-dialog>
</template>

<script setup>
import { defineProps, ref, defineEmits, watch, computed } from 'vue'
import { ElRadioGroup } from 'element-plus'

import { packTypeEnum } from '@enum-ms/mes'
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
  },
  isShowPrice: {
    type: Boolean,
    default: false
  }
})
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true
  },
  dialogVisible
)

const monomerStatus = ref(SummaryStatusEnum.PROCESS.V)
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
          v.totalWeight = convertUnits(v.weight * v.showQuantity, 'kg', 't')
          if (props.isShowPrice) v.totalPrice = v.unitPrice * v.weight || 0
          return v
        })
      )
    case packTypeEnum.ENCLOSURE.V:
      return (
        enclosureList.value &&
        enclosureList.value.map((v) => {
          v.showQuantity = v[props.quantityFelid]
          v.processingPrice = v.processingPrice || v.processingPrice === 0 ? v.processingPrice : undefined
          v.totalLength = convertUnits(v.length * v.showQuantity, 'mm', 'm')
          if (props.isShowPrice) v.totalPrice = v.unitPrice * v.length || 0
          return v
        })
      )
    case packTypeEnum.AUXILIARY_MATERIAL.V:
      return (
        auxList.value &&
        auxList.value.map((v) => {
          v.showQuantity = v[props.quantityFelid]
          v.fullClassName = `${v.firstName}/${v.secondName}/${v.thirdName}`
          if (props.isShowPrice) v.totalPrice = v.unitPrice * v.showQuantity || 0
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
