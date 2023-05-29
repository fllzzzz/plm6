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
      :data-format="dataFormat"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        show-overflow-tooltip
        label="变更编号"
        prop="serialNumber"
        align="center"
        min-width="100"
      />
      <el-table-column v-if="columns.visible('project')" show-overflow-tooltip label="变更项目" prop="project" min-width="150" />
      <el-table-column
        v-if="columns.visible('changeReasonTypeEnum')"
        show-overflow-tooltip
        label="变更原因"
        prop="changeReasonTypeEnum"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('user.name')"
        show-overflow-tooltip
        label="变更提交人"
        prop="user.name"
        align="center"
        width="120"
      />
      <el-table-column
        v-if="columns.visible('createTime')"
        show-overflow-tooltip
        label="变更时间"
        prop="createTime"
        align="center"
        width="170"
      />
      <el-table-column v-if="columns.visible('confirmName')" show-overflow-tooltip label="生产确认人" prop="confirmName" align="center" width="120" />
      <el-table-column v-if="columns.visible('confirmTime')" show-overflow-tooltip label="确认时间" prop="confirmTime" align="center" width="170" />
      <el-table-column v-if="columns.visible('statusEnum')" show-overflow-tooltip label="状态" prop="statusEnum" align="center" width="100">
        <template #default="{ row }">
          <el-tag v-if="row.statusEnum" effect="plain" :type="changeRecordStatusEnum.V[row.statusEnum].T">
            {{ changeRecordStatusEnum.VL[row.statusEnum] }}
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column v-permission="permission.detail" label="操作" width="100px" align="center" fixed="right">
        <template #default="{ row }">
          <common-button
            v-permission="permission.detail"
            size="mini"
            type="primary"
            :disabled="row.statusEnum === changeRecordStatusEnum.CONFIRMED.V"
            icon="el-icon-view"
            @click.stop="toDetail(row)"
          />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mDetail v-model:visible="detailVisible" :info="itemInfo" />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/changed-manage/change-record'
import { ref } from 'vue'

import { changeRecordStatusEnum } from '@enum-ms/production'
import { changeReasonTypeEnum } from '@enum-ms/plan'
import { changeRecordPM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mDetail from './module/detail'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = ref([
  ['createTime', ['parse-time', '{y}-{m}-{d}']],
  ['project', 'parse-project'],
  ['changeReasonTypeEnum', ['parse-enum', changeReasonTypeEnum]]
])

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '变更记录',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    invisibleColumns: [],
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const itemInfo = ref({})
const detailVisible = ref(false)

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    return v
  })
}

function toDetail(row) {
  itemInfo.value = row
  detailVisible.value = true
}
</script>
