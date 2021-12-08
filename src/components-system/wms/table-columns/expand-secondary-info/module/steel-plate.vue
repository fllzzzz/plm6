<template>
  <div class="flex-rbs">
    <div class="info">
      <p v-if="showBrand">
        品牌：<span v-empty-text>{{ props.row.brand }}</span>
      </p>
      <p>
        炉批号：<span v-empty-text>{{ props.row.heatNoAndBatchNo }}</span>
      </p>
      <p v-if="showRemark">
        备注：<span v-empty-text>{{ props.row.remark }}</span>
      </p>
    </div>
    <div class="square-content">
      <steel-plate-square :width="row.width" :length="row.length" />
    </div>
  </div>
</template>

<script setup>
import { defineProps } from 'vue'
import steelPlateSquare from '@/components-system/wms/steel-plate-square.vue'

const props = defineProps({
  row: {
    type: Object,
    default: () => {
      return {}
    }
  },
  showRemark: {
    type: Boolean,
    default: false
  },
  showBrand: {
    type: Boolean,
    default: false
  }
})
</script>

<style lang="scss" scoped>
.square-content {
  margin: 10px 10px 0 10px;
}
</style>
