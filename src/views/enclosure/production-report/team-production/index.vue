<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader class="head-container" />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :data-format="dataFormat"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('project')"
        key="project"
        prop="project"
        :show-overflow-tooltip="true"
        label="项目"
        align="center"
      />
      <el-table-column
        key="workshop.name"
        prop="workshop.name"
        v-if="columns.visible('workshop.name')"
        show-overflow-tooltip
        label="车间"
        align="center"
      />
      <el-table-column
        key="line.name"
        prop="line.name"
        v-if="columns.visible('line.name')"
        show-overflow-tooltip
        label="生产线"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('LeaderName')"
        key="LeaderName"
        prop="LeaderName"
        :show-overflow-tooltip="true"
        label="班组"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('totalLength')"
        key="totalLength"
        prop="totalLength"
        :show-overflow-tooltip="true"
        label="产量(米)"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('price')"
        key="price"
        prop="price"
        :show-overflow-tooltip="true"
        label="工资单价"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('totalAmount')"
        key="totalAmount"
        prop="totalAmount"
        :show-overflow-tooltip="true"
        label="总额"
        align="center"
      />
      <!--编辑与删除-->
      <el-table-column v-if="checkPermission([...permission.detail])" label="操作" width="80px" align="center" fixed="right">
        <template #default="{ row }">
          <common-button size="mini" type="primary" @click="showDetail(row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 查看详情 -->
    <m-detail v-model:visible="detailVisible" :info="itemInfo" :permission="permission" />
  </div>
</template>

<script setup>
import crudApi from '@/api/enclosure/production-report/team-production'
import { ref } from 'vue'

import { enclosureTeamProductionPM as permission } from '@/page-permission/enclosure'
import checkPermission from '@/utils/system/check-permission'
import { tableSummary } from '@/utils/el-extra'
import { DP } from '@/settings/config'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import MDetail from './module/detail'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const detailVisible = ref(false)
const itemInfo = ref({})

const dataFormat = ref([
  ['project', 'parse-project'],
  ['totalLength', ['to-fixed', DP.MES_ENCLOSURE_L__M]],
  ['price', ['to-fixed-ck', 'YUAN']],
  ['totalAmount', ['to-fixed-ck', 'YUAN']]
])

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [
      ['totalAmount', DP.YUAN],
      ['totalLength', DP.MES_ENCLOSURE_L__M]
    ]
  })
}

const { CRUD, crud, columns } = useCRUD(
  {
    title: '班组产量',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  paginate: true
})

function showDetail(row) {
  detailVisible.value = true
  itemInfo.value = {
    ...row,
    startTime: crud.query.startTime,
    endTime: crud.query.endTime
  }
}

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  data.content.forEach((row) => {
    row.totalLength = (row.totalLength || 0) / 1000
    row.totalAmount = (row.price || 0) * row.totalLength
    row.projectId = row.project?.id
  })
}
</script>
