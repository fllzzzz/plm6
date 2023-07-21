<template>
  <component :is="comp" :basicClass="basicClass" :columns="columns" :show-batch-no="showBatchNo" :fixed="fixed" />
  <el-table-column
    v-if="showRemark"
    key="remark"
    prop="remark"
    label="备注"
    align="left"
    min-width="120px"
    :fixed="fixed"
    show-overflow-tooltip
  />
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import steel from './module/steel.vue'
import rawMat from './module/raw-mat.vue'
import { isBlank } from '@/utils/data-type'
import { STEEL_ENUM } from '@/settings/config'

const props = defineProps({
  basicClass: {
    type: Number
  },
  columns: {
    type: Object
  },
  showBatchNo: {
    // 显示炉批号
    type: Boolean,
    default: true
  },
  showRemark: {
    // 显示备注
    type: Boolean,
    default: false
  },
  fixed: {
    // 定位
    type: String
  }
})

const comp = computed(() => {
  if (props.basicClass & STEEL_ENUM) {
    return steel
  }
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
    case rawMatClsEnum.SECTION_STEEL.V:
    case rawMatClsEnum.STEEL_COIL.V:
      return steel
    case rawMatClsEnum.MATERIAL.V:
    case rawMatClsEnum.GAS.V:
    default:
      return rawMat
  }
})

const showRemark = computed(() => props.showRemark && (isBlank(props.columns) || props.columns.visible('remark')))
</script>
