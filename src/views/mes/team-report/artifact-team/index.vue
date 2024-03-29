<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <belonging-info-columns :columns="columns" showFactory showWorkshop fixedWidth />
      <el-table-column
        v-if="columns.visible('productionLine.name')"
        key="productionLine.name"
        prop="productionLine.name"
        :show-overflow-tooltip="true"
        label="生产线"
        width="250px"
      >
        <template v-slot="scope">
          <el-tag :type="componentTypeEnum.V[scope.row.productType].T" effect="plain"> {{ scope.row.productionLine?.name }} > {{ componentTypeEnum.V[scope.row.productType].SL }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column label="完成状态">
        <template v-slot="scope">
          <div class="status-content">
            <div v-for="(item, index) in scope.row.completeStatus" :key="index" class="status-item">
              <el-progress type="circle" :percentage="+item.completeRate" :stroke-width="6" :width="70" :color="colors">
                <template #default="{ percentage }">
                  <div style="display: flex; flex-direction: column">
                    <span class="percentage-label" style="margin-bottom: 5px">{{ item.name }}</span>
                    <span class="percentage-value">{{ toFixed(percentage, 2) }}%</span>
                  </div>
                </template>
              </el-progress>
              <div class="status-detail">
                <div>任务量：{{ item.taskMeteShow }}</div>
                <div>已完成：{{ item.completeMeteShow }}</div>
                <common-button v-permission="permission.processDetail" type="text" size="mini" @click="showItemDetail(item, scope.row)">
                  查看详情
                </common-button>
              </div>
            </div>
          </div>
        </template>
      </el-table-column>
      <el-table-column v-permission="permission.detail" label="操作" width="120px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button type="primary" size="mini" @click="showDetail(scope.row)">全景看板</common-button>
        </template>
      </el-table-column>
    </common-table>
    <mDetail v-model:visible="detailVisible" :info="detailInfo" />
    <item-detail v-model:visible="itemDetailVisible" :info="detailInfo" :item-info="itemDetailInfo" />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/team-report/artifact-team'
import { ref, reactive, provide } from 'vue'

import { artifactTeamReportPM as permission } from '@/page-permission/mes'
import { componentTypeEnum } from '@enum-ms/mes'
import { toFixed } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import mHeader from './module/header'
import mDetail from './module/detail'
import itemDetail from './module/item-detail'

const colors = [
  { color: '#f56c6c', percentage: 30 },
  { color: '#e6a23c', percentage: 70 },
  { color: '#6f7ad3', percentage: 100 }
]

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '结构班组进度',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)
const { maxHeight } = useMaxHeight({ paginate: false })

provide('query', crud.query)

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.completeStatus = v.processSummaryList.map((o) => {
      o.completeMete = useProductMeteConvert({
        productType: v.productType,
        weight: { num: o.completeNetWeight },
        length: { num: o.completeLength }
      })
      o.completeMeteShow = useProductMeteConvert({
        productType: v.productType,
        weight: { num: o.completeNetWeight },
        length: { num: o.completeLength, to: 'm', dp: 'COM_L__M' },
        showUnit: true
      })
      o.taskMete = useProductMeteConvert({
        productType: v.productType,
        weight: { num: o.taskNetWeight },
        length: { num: o.taskLength }
      })
      o.taskMeteShow = useProductMeteConvert({
        productType: v.productType,
        weight: { num: o.taskNetWeight },
        length: { num: o.taskLength, to: 'm', dp: 'COM_L__M' },
        showUnit: true
      })
      o.completeRate = o.taskMete ? (o.completeMete / o.taskMete) * 100 : '0'
      return o
    })
    return v
  })
}

let detailInfo = reactive({})
const detailVisible = ref(false)
function showDetail(row) {
  detailVisible.value = true
  detailInfo = Object.assign(detailInfo, row)
}

let itemDetailInfo = reactive({})
const itemDetailVisible = ref(false)
function showItemDetail(item, row) {
  itemDetailVisible.value = true
  itemDetailInfo = Object.assign(itemDetailInfo, item)
  detailInfo = Object.assign(detailInfo, row)
}
</script>

<style lang="scss" scoped>
.status-content {
  display: flex;
  flex-wrap: wrap;

  .status-item {
    display: flex;
    align-items: center;
    margin-right: 20px;
    padding: 5px 0px;
    box-sizing: border-box;

    .status-detail {
      margin-left: 10px;
      font-size: 12px;
    }
  }
}
</style>

<style lang="scss">
.team-report {
  .el-progress--circle .el-progress__text > span,
  .el-progress--dashboard .el-progress__text {
    white-space: pre-line;
  }
}
</style>
