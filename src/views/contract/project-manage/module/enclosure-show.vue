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
import { ref, defineProps, watch, computed } from 'vue'
import { TechnologyTypeEnum } from '@enum-ms/contract'
import sandwichTable from './enclosure-table/sandwich-table'
import pressedColorTable from './enclosure-table/pressed-color-table'
import pressedSupportTable from './enclosure-table/pressed-support-table'
import structureTable from './enclosure-table/structure-table'
import trussSupportTable from './enclosure-table/truss-support-table'

const props=defineProps({
  tableData: {
    type: Object,
    default: () => {
      return {
        [TechnologyTypeEnum.ENUM.STRUCTURE.V]: [],
        [TechnologyTypeEnum.ENUM.PROFILEDPLATE.V]: [],
        [TechnologyTypeEnum.ENUM.TRUSSFLOORPLATE.V]: [],
        [TechnologyTypeEnum.ENUM.PRESSUREBEARINGPLATE.V]: [],
        [TechnologyTypeEnum.ENUM.SANDWICH_BOARD.V]: []
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
const currentView = computed(()=> {
  switch (boardType.value) {
    case TechnologyTypeEnum.ENUM.STRUCTURE.V: return structureTable
    case TechnologyTypeEnum.ENUM.PROFILEDPLATE.V : return pressedColorTable
    case TechnologyTypeEnum.ENUM.TRUSSFLOORPLATE.V: return trussSupportTable
    case TechnologyTypeEnum.ENUM.PRESSUREBEARINGPLATE.V : return pressedSupportTable
    case TechnologyTypeEnum.ENUM.SANDWICH_BOARD.V: return sandwichTable
    default: return ''
  }
})
// import sandwichTable from './enclosure-table/sandwich-table'
// import pressedColorTable from './enclosure-table/pressed-color-table'
// import pressedSupportTable from './enclosure-table/pressed-support-table'
// import structureTable from './enclosure-table/structure-table'
// import trussSupportTable from './enclosure-table/truss-support-table'
// import { TechnologyTypeEnum as typeEnum } from '@/utils/enum/index'

// export default {
//   components: { sandwichTable, pressedColorTable, pressedSupportTable, structureTable, trussSupportTable },
//   props: {
//     tableData: {
//       type: Object,
//       default: () => {
//         return {
//           [typeEnum.STRUCTURE.V]: [],
//           [typeEnum.PROFILEDPLATE.V]: [],
//           [typeEnum.TRUSSFLOORPLATE.V]: [],
//           [typeEnum.PRESSUREBEARINGPLATE.V]: [],
//           [typeEnum.SANDWICH_BOARD.V]: []
//         }
//       }
//     },
//     showItem: {
//       type: Array,
//       default: () => {
//         return []
//       }
//     }
//   },
//   data() {
//     return {
//       boardType: undefined,
//       typeEnum
//     }
//   },
//   computed: {
//     currentView() {
//       switch (this.boardType) {
//         case typeEnum.STRUCTURE.V: return 'structure-table'
//         case typeEnum.PROFILEDPLATE.V : return 'pressed-color-table'
//         case typeEnum.TRUSSFLOORPLATE.V: return 'truss-support-table'
//         case typeEnum.PRESSUREBEARINGPLATE.V : return 'pressed-support-table'
//         case typeEnum.SANDWICH_BOARD.V: return 'sandwich-table'
//         default: return ''
//       }
//     },
//     copyTableData() {
//       return JSON.parse(JSON.stringify(this.tableData))
//     }
//   },
//   watch: {
//     copyTableData(val) {
//       if (!this.boardType) {
//         this.boardType = typeEnum.SANDWICH_BOARD.V
//         if (val[typeEnum.SANDWICH_BOARD.V].length) {
//           this.boardType = typeEnum.SANDWICH_BOARD.V
//         } else if (val[typeEnum.PRESSED_COLOR_BOARD.V].length) {
//           this.boardType = typeEnum.PRESSED_COLOR_BOARD.V
//         }
//       }
//     }
//   }
// }
</script>

<style lang='scss' scoped>
::v-deep(.el-table .cell){
  padding: 0px;
}

::v-deep(.el-table thead.is-group th){
  background: #fff;
}
</style>
