<template>
  <div class="app-container warehouse-config">
    <!--工具栏-->
    <m-header />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      @selection-change="crud.selectionChangeHandler"
      row-key="id"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        :show-overflow-tooltip="true"
        prop="name"
        label="仓库位置"
        align="left"
        min-width="150"
      />
      <el-table-column
        v-if="columns.visible('materialType')"
        key="materialType"
        :show-overflow-tooltip="true"
        prop="name"
        label="可存储材料类型"
        align="left"
        min-width="300"
      >
        <template #default="{ row }">
          <template v-for="(item, index) in row.materialTypeName" :key="index">
            <el-tag effect="plain" style="margin-right: 5px">{{ item }}</el-tag>
          </template>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('type')"
        key="type"
        :show-overflow-tooltip="true"
        prop="name"
        label="仓库类型"
        align="center"
        width="100px"
      >
        <template #default="{ row }">
          <el-tag :type="row.sourceRow.type === warehouseTypeEnum.NORMAL.V ? 'info' : 'warning'">
            {{ row.type }}
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('sort')"
        key="sort"
        :show-overflow-tooltip="true"
        prop="sort"
        label="排序"
        align="center"
        width="100px"
      />
      <el-table-column
        v-if="columns.visible('enabled')"
        :show-overflow-tooltip="true"
        prop="enabled"
        label="启用状态"
        align="center"
        width="100px"
      >
        <template #default="{ row }">
          <template v-if="checkPermission(permission.edit)">
            <el-switch :disabled="row.enabledLoading" v-model="row.enabled" class="drawer-switch" @change="handleEnabledChange(row)" />
          </template>
          <template v-else>
            {{ enabledEnum.VL[row.enabled] }}
          </template>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建日期" width="140px" />
      <!--编辑与删除-->
      <el-table-column v-permission="[...permission.edit, ...permission.del]" label="操作" width="130px" align="center" fixed="right">
        <template #default="{ row }">
          <udOperation :data="row" />
        </template>
      </el-table-column>
    </common-table>
    <!-- 表单 -->
    <m-form />
    <m-batch-form />
  </div>
</template>

<script setup>
import crudApi, { editEnabled } from '@/api/config/wms/warehouse'
import { configWmsFactoryWarehousePM as permission } from '@/page-permission/config'

import { ref } from 'vue'
import EO from '@enum'
import { matClsEnum } from '@enum-ms/classification'
import { enabledEnum } from '@/utils/enum/modules/common'
import { warehouseTypeEnum } from '@enum-ms/wms'
import { baseTimeColumns } from '@/utils/columns-format/common'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useCrudEnabledChange from '@compos/use-crud-enabled-change'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'
import mBatchForm from './module/batch-form'

const optShow = {
  batchAdd: true,
  add: true,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const columnsDataFormat = [...baseTimeColumns, ['type', ['parse-enum', warehouseTypeEnum]]]
const { maxHeight } = useMaxHeight()

const { CRUD, crud, columns } = useCRUD(
  {
    title: '仓库设置',
    formStore: true,
    formStoreKey: 'CONFIG_WMS_WAREHOUSE',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    sort: ['sort.asc'],
    queryOnPresenterCreated: false
  },
  tableRef
)

const { handleEnabledChange } = useCrudEnabledChange({ CRUD, crud, editEnabled })

CRUD.HOOK.handleRefresh = (crud, { data: { content }}) => {
  content.forEach((v) => {
    const mt = v.materialType
    v.materialTypeName = EO.getBits(matClsEnum.ENUM, mt, 'L')
  })
}
</script>
