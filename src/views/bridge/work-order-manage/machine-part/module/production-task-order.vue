<template>
  <common-table
    v-loading="tableLoading"
    ref="table"
    :data="tableData"
    empty-text="暂无数据"
    :data-format="dataFormat"
    :max-height="maxHeight"
    style="width: 100%"
    show-summary
    :summary-method="getSummaries"
  >
    <el-table-column :show-overflow-tooltip="true" prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column :show-overflow-tooltip="true" prop="project" key="project.shortName" label="项目" min-width="120" />
    <el-table-column :show-overflow-tooltip="true" prop="serialNumber" key="serialNumber" label="编号" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="specification" key="specification" label="规格" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="material" key="material" label="材质" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="quantity" key="quantity" label="数量" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="weight" key="weight" label="重量（kg）" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="picturePath" key="picturePath" label="图形" align="center" width="150">
      <template v-slot="scope">
        <div style="width: 100%; height: 80px;">
          <el-image style="width: 100%; height: 100%;" :src="scope.row.picturePath" fit="scale-down" />
        </div>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps, ref } from 'vue'
import { tableSummary } from '@/utils/el-extra'

defineProps({
  tableData: {
    type: Array,
    default: () => []
  },
  tableLoading: {
    type: Boolean,
    default: false
  },
  maxHeight: {
    type: [String, Number]
  }
})

const dataFormat = ref([['project', 'parse-project']])

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['quantity', 'weight']
  })
}
</script>

<style lang="scss" scoped></style>
