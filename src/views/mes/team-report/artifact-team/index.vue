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
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('workshop.name')"
        key="workshop.name"
        prop="workshop.name"
        :show-overflow-tooltip="true"
        label="车间"
        width="150px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.workshop?.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('productionLine.name')"
        key="productionLine.name"
        prop="productionLine.name"
        :show-overflow-tooltip="true"
        label="生产线"
        width="150px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.productionLine.name }}</span>
        </template>
      </el-table-column>
      <el-table-column label="完成状态">
        <template v-slot="scope">
          <div class="status-content">
            <div v-for="(item, index) in scope.row.completeStatus" :key="index" class="status-item">
              <el-progress type="circle" :percentage="item.completeRate" :stroke-width="6" :width="70" :color="colors">
                <template #default="{ percentage }">
                  <div style="display: flex; flex-direction: column">
                    <span class="percentage-label" style="margin-bottom: 5px">{{ item.processName }}</span>
                    <span class="percentage-value">{{ toFixed(percentage, 2) }}%</span>
                  </div>
                </template>
              </el-progress>
              <div class="status-detail">
                <div>任务量：{{ toFixed(item.taskMete, DP.COM_WT__KG) }}kg</div>
                <div>已完成：{{ toFixed(item.completeMete, DP.COM_WT__KG) }}kg</div>
                <common-button type="text" size="mini" @click="showItemDetail(item,scope.row)">查看详情</common-button>
              </div>
            </div>
          </div>
        </template>
      </el-table-column>
      <el-table-column label="操作" width="120px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button type="primary" size="mini" @click="showDetail(scope.row)">全景看板</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mDetail v-model:visible="detailVisible" :info="detailInfo" />
    <item-detail v-model:visible="itemDetailVisible" :info="detailInfo" :item-info="itemDetailInfo" />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/team-report/artifact-team'
import { ref, reactive } from 'vue'

import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mDetail from './module/detail'
import itemDetail from './module/item-detail'

const colors = [
  { color: '#f56c6c', percentage: 30 },
  { color: '#e6a23c', percentage: 70 },
  { color: '#6f7ad3', percentage: 100 }
]

// crud交由presenter持有
const permission = {
  get: [''],
  edit: [''],
  add: [''],
  del: ['']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '结构班组',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)
const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
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
