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
    <template v-if="projectType !== projectTypeEnum.BRIDGE.V">
      <div class="head-container">
        <div
          style="color: red; margin-bottom: 8px"
          v-if="
            (detailInfo.deliveryStatus === deliveryStatusEnum.RETURN.V ||
              detailInfo.shipmentStatus === deliveryReceiptStatusEnum.RETURN.V) &&
            detailInfo.cancelDeliveryReason
          "
        >
          当前发运已取消，取消原因：{{ detailInfo.cancelDeliveryReason }}
        </div>
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
            :disabled="auxList.length == 0"
            >{{ packTypeEnum.AUXILIARY_MATERIAL.L }}({{ auxList.length }})</el-radio-button
          >
        </el-radio-group>
        <common-radio-button
          v-model="monomerStatus"
          v-if="productType !== packTypeEnum.ENCLOSURE.V"
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
    </template>
    <template v-if="projectType === projectTypeEnum.BRIDGE.V">
      <div class="head-container">
        <div
          style="color: red; margin-bottom: 8px"
          v-if="
            (detailInfo.deliveryStatus === deliveryStatusEnum.RETURN.V ||
              detailInfo.shipmentStatus === deliveryReceiptStatusEnum.RETURN.V) &&
            detailInfo.cancelDeliveryReason
          "
        >
          当前发运已取消，取消原因：{{ detailInfo.cancelDeliveryReason }}
        </div>
        <el-radio-group v-model="bridgeCurProductType" v-if="bridgeProductTypeBits.length > 1" size="small" class="filter-item">
          <el-radio-button
            v-if="bridgePackTypeEnum.BOX.V & productType"
            :label="bridgePackTypeEnum.BOX.V"
            :disabled="boxList.length == 0"
            >{{ bridgePackTypeEnum.BOX.L }}({{ boxList.length }})</el-radio-button
          >
          <el-radio-button
            v-if="bridgePackTypeEnum.MACHINE_PART.V & productType"
            :label="bridgePackTypeEnum.MACHINE_PART.V"
            :disabled="partList.length == 0"
            >{{ bridgePackTypeEnum.MACHINE_PART.L }}({{ partList.length }})</el-radio-button
          >
          <el-radio-button
            v-if="bridgePackTypeEnum.AUXILIARY_MATERIAL.V & productType"
            :label="bridgePackTypeEnum.AUXILIARY_MATERIAL.V"
            :disabled="auxList.length == 0"
            >{{ bridgePackTypeEnum.AUXILIARY_MATERIAL.L }}({{ auxList.length }})</el-radio-button
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
        :is="bridgeCurrentView"
        v-loading="tableLoading"
        :maxHeight="maxHeight"
        :list="bridgeList"
        :isSuspend="monomerStatus === SummaryStatusEnum.SUSPEND.V"
      />
    </template>
  </common-dialog>
</template>

<script setup>
import { defineProps, ref, defineEmits, watch, computed } from 'vue'
import { ElRadioGroup } from 'element-plus'
import { projectTypeEnum } from '@enum-ms/contract'
import { packTypeEnum, deliveryStatusEnum, deliveryReceiptStatusEnum } from '@enum-ms/mes'
import { bridgePackTypeEnum } from '@enum-ms/bridge'
import { weightTypeEnum } from '@enum-ms/common'
import { convertUnits } from '@/utils/convert/unit'
import EO from '@enum'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import structureTable from './module/structure'
import partTable from './module/part'
import enclosureTable from './module/enclosure'
import auxiliaryMaterialTable from './module/auxiliary-material'
import boxTable from './module/box'

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
  },
  projectType: {
    type: Number
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
const artifactList = ref([])
const partList = ref([])
const enclosureList = ref([])
const auxList = ref([])
const boxList = ref([])
const curProductType = ref()
const bridgeCurProductType = ref()

const productType = computed(() => {
  return props.detailInfo && props.detailInfo.productType
})

const productTypeBits = computed(() => {
  return EO.getBits(packTypeEnum, productType.value, 'V')
})
// 桥梁
const bridgeProductTypeBits = computed(() => {
  return EO.getBits(bridgePackTypeEnum, productType.value, 'V')
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
          v.totalWeight = convertUnits(v.weight * v.showQuantity, 'kg', 't')
          return v
        })
      )
    case packTypeEnum.MACHINE_PART.V:
      return (
        partList.value &&
        partList.value.map((v) => {
          v.showQuantity = v[props.quantityFelid]
          v.weight = (props.weightType === weightTypeEnum.NET.V ? v.netWeight : v.grossWeight) || 0
          v.totalWeight = convertUnits(v.weight * v.showQuantity, 'kg', 't')
          return v
        })
      )
    case packTypeEnum.ENCLOSURE.V:
      return (
        enclosureList.value &&
        enclosureList.value.map((v) => {
          v.showQuantity = v[props.quantityFelid]
          v.totalLength = convertUnits(v.length * v.showQuantity, 'mm', 'm')
          return v
        })
      )
    case packTypeEnum.AUXILIARY_MATERIAL.V:
      return (
        auxList.value &&
        auxList.value.map((v) => {
          v.showQuantity = v[props.quantityFelid]
          v.fullClassName = `${v.firstName}/${v.secondName}/${v.thirdName}`
          return v
        })
      )
    default:
      return []
  }
})
// 桥梁
const bridgeCurrentView = computed(() => {
  switch (bridgeCurProductType.value) {
    case bridgePackTypeEnum.BOX.V:
      return boxTable
    case bridgePackTypeEnum.MACHINE_PART.V:
      return partTable
    case bridgePackTypeEnum.AUXILIARY_MATERIAL.V:
      return auxiliaryMaterialTable
    default:
      return ''
  }
})
const bridgeList = computed(() => {
  switch (bridgeCurProductType.value) {
    case bridgePackTypeEnum.BOX.V:
      return (
        boxList.value &&
        boxList.value.map((v) => {
          v.showQuantity = v[props.quantityFelid]
          v.weight = (props.weightType === weightTypeEnum.NET.V ? v.netWeight : v.grossWeight) || 0
          v.totalWeight = convertUnits(v.weight * v.showQuantity, 'kg', 't')
          return v
        })
      )
    case bridgePackTypeEnum.MACHINE_PART.V:
      return (
        partList.value &&
        partList.value.map((v) => {
          v.showQuantity = v[props.quantityFelid]
          v.weight = (props.weightType === weightTypeEnum.NET.V ? v.netWeight : v.grossWeight) || 0
          v.totalWeight = convertUnits(v.weight * v.showQuantity, 'kg', 't')
          return v
        })
      )
    case bridgePackTypeEnum.AUXILIARY_MATERIAL.V:
      return (
        auxList.value &&
        auxList.value.map((v) => {
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
    bridgeCurProductType.value = bridgeProductTypeBits.value[0]
    const data = await props.detailFunc(detailId.value)
    console.log(data, 'data')
    emit('getDetail', detailId.value, data)
    artifactList.value = data.artifactList || []
    partList.value = data.partList || []
    enclosureList.value = data.enclosureList || []
    auxList.value = data.auxiliaryMaterialList || []
    boxList.value = data.boxList || []
  } catch (error) {
    console.log('详情', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
