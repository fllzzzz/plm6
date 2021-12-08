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
      style="width: 100%"
      @row-dblclick="(row) => crud.toDetail(row)"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
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
        v-if="columns.visible('applicantName')"
        key="applicantName"
        :show-overflow-tooltip="true"
        prop="applicantName"
        label="申请人"
        align="center"
        width="150"
      />
      <el-table-column
        v-if="columns.visible('reviewerName')"
        key="reviewerName"
        :show-overflow-tooltip="true"
        prop="reviewerName"
        label="处理人"
        align="center"
        width="150"
      />
      <el-table-column
        v-if="columns.visible('userUpdateTime')"
        key="userUpdateTime"
        :show-overflow-tooltip="true"
        prop="userUpdateTime"
        label="申请日期"
        align="center"
        width="160"
        sortable="custom"
      >
        <template #default="{ row }">
          <span v-parse-time>{{ row.userUpdateTime }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('reviewTime')"
        key="reviewTime"
        :show-overflow-tooltip="true"
        prop="reviewTime"
        label="审核日期"
        align="center"
        width="140"
      >
        <template #default="{ row }">
          <span v-parse-time v-empty-text>{{ row.reviewTime }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column label="操作" min-width="180px" align="left">
        <template #default="{ row }">
          <udOperation :show-edit="false" :show-del="false" show-detail :data="row" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 查看详情 -->
    <m-detail />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/outbound/raw-mat-application-record'
import { ref } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation.vue'
import mHeader from './module/header'
import mDetail from './module/detail.vue'
// crud交由presenter持有
const permission = {
  get: ['wms_outboundApplication_record:get']
}

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '出库记录',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })
</script>
