<template>
  <!--  <div v-if="tableData[typeEnum.SANDWICH_BOARD.V].length || tableData[typeEnum.PRESSED_COLOR_BOARD.V].length || tableData[typeEnum.FLOOR_BOARD.V].length">-->
  <div>
    <el-radio-group v-model="boardType" size="small" class="filter-item">
      <el-radio-button
        v-for="item in TechnologyTypeEnum.ENUM"
        :key="item.V"
        :label="item.V"
        :disabled="showItem.indexOf(item.V)<0"
      >
        {{ item.L }}{{ tableData[item.V] && tableData[item.V].length ? `(${tableData[item.V].length})`:'' }}
      </el-radio-button>
    </el-radio-group>
    <!-- 表格 -->
    <component
      :is="currentView"
      :table-data="tableData[boardType]"
      :is-show="true"
      style="margin-top:20px;"
    />
  </div>
</template>

<script setup>
import { ref, defineProps, computed, watch } from 'vue'
import { TechnologyTypeEnum } from '@enum-ms/contract'
import { ElRadioGroup } from 'element-plus'
import sandwichTable from './enclosure-table/sandwich-table'
import pressedColorTable from './enclosure-table/pressed-color-table'
import pressedSupportTable from './enclosure-table/pressed-support-table'
import structureTable from './enclosure-table/structure-table'
import trussSupportTable from './enclosure-table/truss-support-table'

const props = defineProps({
  tableData: {
    type: Object,
    default: () => {
      return {
        [TechnologyTypeEnum.STRUCTURE.V]: [],
        [TechnologyTypeEnum.PROFILED_PLATE.V]: [],
        [TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V]: [],
        [TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V]: [],
        [TechnologyTypeEnum.SANDWICH_BOARD.V]: []
      }
    }
  },
  showItem: {
    type: Array,
    default: () => {
      return []
    }
  }
})
const boardType = ref()

watch(
  () => props.showItem,
  (val) => {
    if (val.length > 0) {
      boardType.value = props.showItem[0]
    }
  },
  { deep: true, immediate: true }
)

const currentView = computed(() => {
  switch (boardType.value) {
    case TechnologyTypeEnum.STRUCTURE.V: return structureTable
    case TechnologyTypeEnum.PROFILED_PLATE.V : return pressedColorTable
    case TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V: return trussSupportTable
    case TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V : return pressedSupportTable
    case TechnologyTypeEnum.SANDWICH_BOARD.V: return sandwichTable
    default: return ''
  }
})
</script>

<style lang='scss' scoped>
::v-deep(.el-table .cell){
  padding: 0px;
}

::v-deep(.el-table thead.is-group th){
  background: #fff;
}
</style>
