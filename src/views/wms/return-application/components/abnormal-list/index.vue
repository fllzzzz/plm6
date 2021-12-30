<template>
  <common-table ref="tableRef" :data="list" :max-height="maxHeight" :expand-row-keys="expandRowKeys" row-key="id">
    <el-expand-table-column :data="list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
      <template #default="{ row }">
        <expand-secondary-info :basic-class="row.basicClass" :row="row" :show-batch-no="false" show-remark show-graphics />
      </template>
    </el-expand-table-column>
    <!-- 基础信息 -->
    <material-base-info-columns
      :basic-class="basicClass"
      :show-length="![rawMatClsEnum.STEEL_PLATE.V, rawMatClsEnum.SECTION_STEEL.V].includes(basicClass)"
      :show-width="basicClass !== rawMatClsEnum.STEEL_PLATE.V"
      :show-thickness="basicClass !== rawMatClsEnum.STEEL_PLATE.V"
      fixed="left"
    />
    <!-- 次要信息 -->
    <material-secondary-info-columns :basic-class="basicClass" />
    <!-- 尺寸信息 -->
    <size-info :basic-class="props.basicClass" />
    <!-- 单位及其数量 -->
    <quantity-info :basic-class="props.basicClass" :show-quantity="basicClass !== rawMatClsEnum.STEEL_COIL.V" />
    <!-- 仓库信息 -->
    <warehouse-info-columns show-project />
  </common-table>
</template>

<script setup>
import { defineProps, ref } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

import QuantityInfo from './module/quantity-info.vue'
import SizeInfo from './module/size-info.vue'

import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
// import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'

const props = defineProps({
  basicClass: {
    // 基础分类
    type: Number
  },
  list: {
    type: Array,
    default: () => []
  },
  maxHeight: {
    type: Number
  }
})

const expandRowKeys = ref([])
</script>

<style lang="scss" scoped>
::v-deep(.important-info) {
  color: #e6a23c;
}

::v-deep(.returnable-number) {
  color: #67c23a;
}

.return-material-info {
  width: 100%;
  padding: 5px 0;
  border: 1px dashed #ccc;
  ::v-deep(.info-item) {
    display: inline-block;
    margin: 5px 0;
    font-size: 13px;
    min-width: 180px;
    // color: brown;
    // font-weight: bold;
    // width: 100px;
    > span {
      display: inline-block;
      overflow: hidden;
    }
    > span:first-child {
      font-weight: bold;
      width: 80px;
      text-align: right;
      &:after {
        content: '：';
      }
    }
    > span:last-child {
      text-overflow: ellipsis;
      white-space: nowrap;
      // display: inline;
      // color: #ccc;
      // font-weight: normal;
    }
  }
}
</style>
