<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader ref="header" :permission="permission" />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :show-empty-symbol="false"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('name')" key="name" :show-overflow-tooltip="true" prop="name" label="名称" align="center" />
      <el-table-column
        v-if="columns.visible('symbol')"
        key="symbol"
        :show-overflow-tooltip="true"
        prop="symbol"
        label="符号"
        align="center"
      />
      <el-table-column v-if="columns.visible('type')" :show-overflow-tooltip="true" prop="type" label="类型" align="center">
        <template #default="{ row }">
          {{ unitTypeEnum.VL[row.type] }}
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('enabled')" :show-overflow-tooltip="true" prop="enabled" label="启用状态" align="center">
        <template #default="{ row }">
          <template v-if="checkPermission(permission.edit)">
            <el-switch :disabled="row.enabledLoading" v-model="row.enabled" class="drawer-switch" @change="handleEnabledChange(row)" />
          </template>
          <template v-else>
            {{ enabledEnum.VL[row.enabled] }}
          </template>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('updateTime')" key="updateTime" prop="updateTime" label="编辑日期" width="140px" />
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建日期" width="140px" />
      <!--编辑与删除-->
      <el-table-column v-permission="permission.del" label="操作" width="130px" align="center">
        <template #default="{ row }">
          <udOperation :show-del="!row.boolSystem" :show-edit="false" :data="row" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <m-batch-form />
  </div>
</template>

<script setup>
// 系统字段添加标识
import crudApi, { editEnabled } from '@/api/config/main/unit-config'
import { configUnitPM as permission } from '@/page-permission/config'

import { ref } from 'vue'
import { enabledEnum, unitTypeEnum } from '@enum-ms/common'
import { baseTimeColumns } from '@/utils/columns-format/common'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useCrudEnabledChange from '@compos/use-crud-enabled-change'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mBatchForm from './module/batch-form'

const optShow = {
  batchAdd: true,
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
// 表格列数据格式转换
const columnsDataFormat = ref([...baseTimeColumns])
const { CRUD, crud, columns } = useCRUD(
  {
    title: '基础单位配置',
    formStore: true,
    formStoreKey: 'BASE_CONFIG_UNIT',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

// 启用状态变更
const { handleEnabledChange } = useCrudEnabledChange({ CRUD, crud, editEnabled })
</script>
