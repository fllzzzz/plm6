<template>
<div class="app-container">
  <div v-show="!detailRow.id" class="my-code" style="width: 100%">*点击左侧表格行查看详情</div>
  <div v-show="detailRow.id" class="production-detail" style="width: 100%">
    <common-table
      ref="tableRef"
      :data="workshopList"
      :empty-text="'暂无数据'"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      :span-method="spanMethod"
    >
      <el-table-column align="center" key="workshop" prop="workshop" :show-overflow-tooltip="true" label="车间/产线">
        <template v-slot="scope">
          <span>{{ scope.row.workshop }}/{{ scope.row.productionLine }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="team" prop="team" :show-overflow-tooltip="true" label="班组">
        <template v-slot="scope">
          <span>{{ scope.row.team }}（{{scope.row.userName}}）</span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="production" prop="production" :show-overflow-tooltip="true" label="产量（件/吨）">
        <template v-slot="scope">
          <span>{{ scope.row.productionQuantity }}/{{ scope.row.productionWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="wage" prop="wage" :show-overflow-tooltip="true" label="工资（元）">
        <template v-slot="scope">
          <span>{{ scope.row.wage }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="productionProportion" prop="productionProportion" :show-overflow-tooltip="true" label="产量占比">
        <template v-slot="scope">
          <el-progress :text-inside="true" stroke-linecap="square" :stroke-width="22" :percentage="scope.row.productionProportion" status="success" />
        </template>
      </el-table-column>
      <el-table-column align="center" :show-overflow-tooltip="true" label="操作">
        <template v-slot="scope">
          <common-button type="primary" size="mini" @click="views(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <common-drawer
      append-to-body
      v-model="visible"
      top="10vh"
      :before-close="() => (visible = false)"
      :title="`工序: ${props.detailRow.process}`"
      :wrapper-closable="false"
      size="80%"
    >
      <template #titleAfter>
        <el-tag style="font-weight: 700">班组：{{productionData.team}}>{{productionData.userName}}</el-tag>
      </template>
      <template #titleRight>
        <print-table :api-key="apiKey" :params="{ ...query }" size="mini" type="warning" class="filter-item" />
      </template>
      <template #content>
        <artifact v-if="props.detailRow.process === '下料'" :visible="visible" :production-data="productionData" />
        <part v-else-if="props.detailRow.process === '组立'" v-model="visible" :production-data="productionData" />
        <perforate v-else-if="props.detailRow.process === '埋弧'" v-model="visible" :production-data="productionData" />
        <lively v-else-if="props.detailRow.process === '焊接'" v-model="visible" :production-data="productionData" />
      </template>
    </common-drawer>
  </div>
  </div>
</template>

<script setup>
import { ref, defineProps } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import artifact from '../module/artifact.vue'
import part from '../module/part.vue'
import perforate from '../module/perforate.vue'
import lively from '../module/livery.vue'

const props = defineProps({
  detailRow: {
    type: Object,
    default: () => {}
  }
})
const { maxHeight } = useMaxHeight({
  wrapperBox: ['.production-detail'],
  paginate: true
})

const visible = ref(false)
const productionData = ref({})
const workshopList = [
  {
    workshop: '一车间',
    productionLine: '一线',
    team: '一组',
    userName: '超级管理员',
    productionQuantity: 60,
    productionWeight: 1000,
    wage: 100,
    productionProportion: 60,
    process: '组立'
  },
  {
    workshop: '一车间',
    productionLine: '一线',
    team: '二组',
    userName: '超级管理员',
    productionQuantity: 60,
    productionWeight: 1000,
    wage: 100,
    productionProportion: 45,
    process: '下料'
  },

  {
    workshop: '一车间',
    productionLine: '二线',
    team: '一组',
    userName: '超级管理员',
    productionQuantity: 60,
    productionWeight: 1000,
    wage: 100,
    productionProportion: 60,
    process: '焊接'
  },
  {
    workshop: '一车间',
    productionLine: '二线',
    team: '二组',
    userName: '超级管理员',
    productionQuantity: 60,
    productionWeight: 1000,
    wage: 100,
    productionProportion: 45,
    process: '涂装'
  }
]
// 查看
function views(row) {
  visible.value = true
  productionData.value = row
}

// 合并的行
function spanMethod({ row, rowIndex, column, columnIndex }) {
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
.app-container {
  padding: 0;
}
</style>
