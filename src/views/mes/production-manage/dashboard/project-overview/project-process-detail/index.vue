<template>
  <div>
    <div v-show="!props.processData.projectId">
      <div class="my-code">点击左边表项目行查看详情</div>
    </div>
    <div v-show="props.processData.projectId">
    <div class="head-container">
      <monomer-select-area-select
      v-model:monomerId="monomerId"
      v-model:areaId="areaId"
      :productType="productType"
      needConvert
      clearable
      :project-id="props.processData.projectId"
      @change="handleMonomerAreaChange"
    />
     <!-- <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
    <common-button
      class="filter-item"
      size="mini"
      type="warning"
      icon="el-icon-refresh-left"
      @click.stop="resetQuery"
    >
      重置
    </common-button> -->
    </div>
    <common-table
      ref="tableRef"
      :data="processList"
      :empty-text="emptyText"
      :max-height="maxHeight"
      row-key="projectId"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60px" type="index" />
      <el-table-column
        align="center"
        key="process"
        prop="process"
        :show-overflow-tooltip="true"
        label="涉及工序"
        width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.process }}</span>
        </template>
      </el-table-column>
      <el-table-column
        align="center"
        key="quantity"
        prop="quantity"
        :show-overflow-tooltip="true"
        label="清单数（件）"
      >
        <template v-slot="scope">
          <span>{{ scope.row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        align="center"
        key="weight"
        prop="weight"
        :show-overflow-tooltip="true"
        label="清单量（kg）"
      >
        <template v-slot="scope">
          <span>{{ scope.row.weight }}</span>
        </template>
      </el-table-column>
      <el-table-column
        align="center"
        key="completeQuantity"
        prop="completeQuantity"
        :show-overflow-tooltip="true"
        label="完成（件）"
      >
        <template v-slot="scope">
          <span>{{ scope.row.completeQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        align="center"
        key="rate"
        prop="rate"
        :show-overflow-tooltip="true"
        label="完成率"
        min-width="150px"
      >
        <template v-slot="scope">
          <el-progress
            :text-inside="true"
            :stroke-width="26"
            :percentage="scope.row.rate"
            status="success"
            />
        </template>
      </el-table-column>
       <el-table-column
        align="center"
        :show-overflow-tooltip="true"
        label="操作"
      >
        <template v-slot="scope">
          <common-button type="primary" size="mini" @click.stop="showDetail(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <process-detail v-model:visible="dialogVisible" :detail-data="detailData" />
    </div>
  </div>
</template>
<script setup>
import { ref, defineProps } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import processDetail from '../process-detail/index.vue'

const tableRef = ref()
const processList = ref([])
const detailData = ref([])
const dialogVisible = ref(false)
const props = defineProps({
  processData: {
    type: Object,
    default: () => {}
  }
})

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

// function searchQuery() {
//   console.log('搜索')
// }
// function resetQuery() {
//   console.log('重置')
// }

function showDetail(row) {
    dialogVisible.value = true
    detailData.value = row
}
</script>
<style lang="scss" scoped>
.app-container {
    padding: 0;
}
</style>
