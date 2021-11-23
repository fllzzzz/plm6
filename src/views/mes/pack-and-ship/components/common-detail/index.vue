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
    <template #tip>
      <slot name="tip"/>
    </template>
    <div class="head-container">
      <el-radio-group v-model="monomerStatus" class="filter-item" size="small">
        <el-radio-button :label="SummaryStatusEnum.PROCESS.V">{{ SummaryStatusEnum.PROCESS.L }}</el-radio-button>
        <el-radio-button :label="SummaryStatusEnum.SUSPEND.V">{{ SummaryStatusEnum.SUSPEND.L }}</el-radio-button>
      </el-radio-group>
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

import { packTypeEnum } from '@enum-ms/mes'
import { weightTypeEnum } from '@enum-ms/common'
import { convertUnits } from '@/utils/convert/unit'

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

const productType = computed(() => {
  return props.detailInfo && props.detailInfo.productType
})
const detailId = computed(() => {
  return (props.detailInfo && props.detailInfo.id) || undefined
})
const currentView = computed(() => {
  switch (productType.value) {
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
  switch (productType.value) {
    case packTypeEnum.STRUCTURE.V:
      return (
        artifactList.value &&
        artifactList.value.map((v) => {
          v.weight = (props.weightType === weightTypeEnum.NET.V ? v.netWeight : v.grossWeight) || 0
          v.totalWeight = convertUnits(v.weight * v.packageQuantity, 'kg', 't')
          return v
        })
      )
    case packTypeEnum.ENCLOSURE.V:
      return (
        enclosureList.value &&
        enclosureList.value.map((v) => {
          v.processingPrice = v.processingPrice || v.processingPrice === 0 ? v.processingPrice : undefined
          v.totalLength = convertUnits(v.length * v.packageQuantity, 'mm', 'm')
          return v
        })
      )
    case packTypeEnum.AUXILIARY_MATERIAL.V:
      return (
        auxList.value &&
        auxList.value.map((v) => {
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
    const data = await props.detailFunc(detailId.value)
    artifactList.value = data.artifactList || []
    enclosureList.value = data.enclosureList || []
    auxList.value = data.auxList || []
  } catch (error) {
    console.log('详情', error)
  } finally {
    tableLoading.value = false
  }
}
</script>
