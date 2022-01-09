<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id">
        <template #default="{ row }">
          <p>关联项目：<span v-parse-project="{ project: row.projects }" v-empty-text /></p>
        </template>
      </el-expand-table-column>
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('applicationSN')"
        key="applicationSN"
        :show-overflow-tooltip="true"
        prop="applicationSN"
        width="160"
        label="出库申请编号"
      />
      <el-table-column
        v-if="columns.visible('basicClass')"
        key="basicClass"
        :show-overflow-tooltip="true"
        prop="basicClass"
        label="物料种类"
        width="200"
      >
        <template #default="{ row }">
          <span v-parse-enum="{ e: matClsEnum.ENUM, v: row.basicClass, bit: true }" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('projects')"
        show-overflow-tooltip
        key="projects"
        prop="projects"
        label="关联项目"
        min-width="170"
      >
        <template #default="{ row }">
          <span v-parse-project="{ project: row.projects, onlyShortName: true }" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('applicantName')"
        key="applicantName"
        :show-overflow-tooltip="true"
        prop="applicantName"
        label="申请人"
        align="center"
        width="140"
      />
      <el-table-column
        v-if="columns.visible('outboundTime')"
        key="outboundTime"
        :show-overflow-tooltip="true"
        prop="outboundTime"
        label="出库日期"
        align="center"
        width="200"
      >
        <template #default="{ row }">
          <span v-parse-time="{ val: row.outboundEarliestTime, fmt: '{y}-{m}-{d}' }" />
          &nbsp;~&nbsp;
          <span v-parse-time="{ val: row.outboundLatestTime, fmt: '{y}-{m}-{d}' }" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="申请日期"
        align="center"
        width="160"
        sortable="custom"
      >
        <template #default="{ row }">
          <span v-parse-time>{{ row.createTime }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column v-permission="permission.review" label="操作" width="120px" align="center" fixed="right">
        <template #default="{ row }">
          <udOperation :data="row" show-detail :show-edit="false" :show-del="false" />
          <common-button type="warning" icon="el-icon-s-check" size="mini" @click="toReview(row)" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 审核 -->
    <review v-model="reviewVisible" :data="currentRow" @refresh="crud.refresh" />
    <m-detail />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/material-outbound/raw-material/review'
import { ref } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import UdOperation from '@crud/UD.operation.vue'
import Pagination from '@crud/Pagination'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import MHeader from './module/header'
import MDetail from './module/detail.vue'
import Review from './module/review.vue'

// crud交由presenter持有
const permission = {
  get: ['wms_outboundApplication_review:get'],
  review: ['wms_outboundApplication_review:review'],
  detail: ['wms_outboundApplication_review:detail']
}

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

const currentRow = ref({})
const reviewVisible = ref(false)
const expandRowKeys = ref([])
const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '出库清单',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

// 打开审核
function toReview(row) {
  currentRow.value = row
  reviewVisible.value = true
}
</script>
