<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
        <template #default="{ row }">
          <!-- <p>关联项目：<span v-parse-project="{ project: row.project }" v-empty-text /></p> -->
          <p>
            调拨备注：<span v-empty-text>{{ row.remark }}</span>
          </p>
        </template>
      </el-expand-table-column>
      <!-- <el-table-column label="序号" type="index" align="center" width="60" /> -->
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="crud.query.basicClass" :show-party-a="false" />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :columns="columns" />
      <el-table-column v-if="columns.visible('project')" show-overflow-tooltip key="project" prop="project" label="原项目" min-width="170">
        <template #default="{ row }">
          <span v-parse-project="{ project: row.project, onlyShortName: true }" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('project')"
        show-overflow-tooltip
        key="project"
        prop="project"
        label="借用项目"
        min-width="170"
      >
        <template #default="{ row }">
          <span v-parse-project="{ project: row.project, onlyShortName: true }" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('transferorName')"
        key="transferorName"
        :show-overflow-tooltip="true"
        prop="transferorName"
        label="调拨人"
        align="center"
        width="90"
      />
      <el-table-column
        v-if="columns.visible('transferTime')"
        key="transferTime"
        :show-overflow-tooltip="true"
        prop="transferTime"
        label="调拨日期"
        align="center"
        width="100"
      >
        <template #default="{ row }">
          <span v-parse-time="'{y}-{m}-{d}'">{{ row.transferTime }}</span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/transfer/party-a-borrow-manage'
import { ref } from 'vue'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'

const permission = {
  get: ['wms_partyABorrow:get'],
  return: ['wms_partyABorrow:return'],
  transferDetail: ['wms_transferApplication_review:detail']
}

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { CRUD, crud, columns } = useCRUD(
  {
    title: '甲供物料借出管理',
    sort: ['id.desc'],
    invisibleColumns: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const expandRowKeys = ref([])
const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content, {
    toSmallest: false,
    toNum: false
  })
}

// TODO:打开入库详情窗口
// function openInboundDetailView() {}
</script>
