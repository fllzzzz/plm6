<template>
  <div>
    <common-table
      ref="tableRef"
      :data="workshopList"
      :empty-text="'暂无数据'"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      :span-method="spanMethod"
    >
      <el-table-column align="center" key="monomerName" prop="monomerName" :show-overflow-tooltip="true" label="单体">
        <template v-slot="scope">
          <span>{{ scope.row.monomerName }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="areaName" prop="areaName" :show-overflow-tooltip="true" label="区域">
        <template v-slot="scope">
          <table-cell-tag show="false" :name="projectNestingStatusEnum.VL[scope.row.nestingStatusEnum]" :color="projectNestingStatusEnum.V[scope.row.nestingStatusEnum].COLOR" :offset="15" />
          <span>{{ scope.row.areaName }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="endDate" prop="endDate" :show-overflow-tooltip="true" label="完成日期">
        <template v-slot="scope">
          <span>{{ scope.row.endDate }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="listEditor" prop="listEditor" :show-overflow-tooltip="true" label="清单编辑">
        <template v-slot="scope">
         <span>{{ scope.row.listEditor }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="assembleQuantity" prop="assembleQuantity" :show-overflow-tooltip="true" label="部件数量">
        <template v-slot="scope">
         <span>{{ scope.row.assembleQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="assembleWeight" prop="assembleWeight" :show-overflow-tooltip="true" label="部件重量">
        <template v-slot="scope">
          <span>{{ scope.row.assembleWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" :show-overflow-tooltip="true" label="操作">
        <template v-slot="scope">
          <common-button type="primary" size="mini" @click="views(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <list-detail v-model:visible="innerVisible" />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import { projectNestingStatusEnum } from '@enum-ms/mes'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'
import listDetail from '../list-detail/index.vue'

const { maxHeight } = useMaxHeight({
  paginate: true
})
const innerVisible = ref(false)
const productionData = ref({})
const workshopList = [
  {
    monomerName: '一号楼',
    areaName: '第一批',
    endDate: 60,
    productionWeight: 1000,
    listEditor: 100,
    assembleQuantity: 60,
    assembleWeight: 1000,
    process: '组立',
    nestingStatusEnum: 1
  },
  {
    monomerName: '一号楼',
    areaName: '第二批',
    endDate: 60,
    productionWeight: 1000,
    listEditor: 100,
    assembleQuantity: 45,
    assembleWeight: 1000,
    process: '下料',
    nestingStatusEnum: 4
  },
  {
    monomerName: '二号楼',
    areaName: '第一批',
    endDate: 60,
    productionWeight: 1000,
    listEditor: 100,
    assembleQuantity: 35,
    assembleWeight: 1000,
    process: '钻孔',
    nestingStatusEnum: 1
  },
  {
    monomerName: '二号楼',
    areaName: '第二批',
    endDate: 60,
    productionWeight: 1000,
    listEditor: 100,
    assembleQuantity: 50,
    assembleWeight: 1000,
    process: '涂装',
    nestingStatusEnum: 4
  }
]

function views(row) {
  console.log(row, 'row')
  innerVisible.value = true
  productionData.value = row
}

// 合并行
function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (columnIndex === 0) {
    if (rowIndex % 2 === 0) {
      return {
        rowspan: 2,
        colspan: 1
      }
    } else {
      return {
        rowspan: 0,
        colspan: 0
      }
    }
  }
}
</script>

<style lang="scss" scoped>
</style>
