<template>
  <el-table-column v-if="showIndex" label="序号" type="index" align="center" width="55" :fixed="fixed" />
  <component :is="comp" :basicClass="basicClass" :columns="columns" :fixed="fixed" />
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { manufClsEnum } from '@/utils/enum/modules/classification'
import structure from './module/structure.vue'
import enclosure from './module/enclosure.vue'

const props = defineProps({
  basicClass: {
    type: Number
  },
  columns: {
    type: Object
  },
  fixed: {
    // 定位
    type: String
  },
  showIndex: {
    // 显示 “序号”
    type: Boolean,
    default: true
  }
})

const comp = computed(() => {
  switch (props.basicClass) {
    case manufClsEnum.STRUC_MANUFACTURED.V:
      return structure
    case manufClsEnum.ENCL_MANUFACTURED.V:
      return enclosure
    default:
      return structure
  }
})
</script>
