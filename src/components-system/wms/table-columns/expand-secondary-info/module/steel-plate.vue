<template>
  <div class="flex-rbs">
    <div class="info">
      <p v-if="showBrand">
        品牌：<span>{{ props.row.brand }}</span>
      </p>
      <p v-if="showBatchNo">
        炉批号：<span>{{ props.row.heatNoAndBatchNo }}</span>
      </p>
      <p v-if="showRemark">
        备注：<span>{{ props.row.remark }}</span>
      </p>
      <slot />
    </div>
    <div class="square-content">
      <div v-if="showGraphics">
        <a v-if="row.surplusGraphicsPath" download target="_blank" :href="row.surplusGraphicsPath">
          <img :src="row.surplusGraphicsPath" style="max-height: 100px;" />
        </a>
        <steel-plate-square v-else :width="row.width" :length="row.length" />
      </div>
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
  },
  showBatchNo: {
    // 显示炉批号
    type: Boolean,
    default: true
  },
  showGraphics: { // 显示钢板图形
    type: Boolean,
    default: false
  }
})
</script>

<style lang="scss" scoped>
.square-content {
  flex: none;
  margin: 10px 10px 10px 10px;
  max-width: 1000px;
  overflow: auto;
}

.flex-rbs {
  margin-top: 12px;
  margin-bottom: 12px;
}
.info {
  flex: auto;
  min-width: 200px;
  max-width: 1000px;
  p {
    margin: 0;
    word-break: break-all;
  }
  >p:nth-child(n){
    margin-top: 12px;
  }
  >p:first-child{
    margin-top: 0;
  }
}
</style>
