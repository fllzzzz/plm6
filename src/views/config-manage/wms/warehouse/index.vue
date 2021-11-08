<template>
  <div class="app-container warehouse-config">
    <!--工具栏-->
    <m-header />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :max-height="maxHeight"
      style="width: 100%"
      @selection-change="crud.selectionChangeHandler"
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
        <template v-slot="scope">
          <template v-for="(item, index) in scope.row.materialType" :key="index">
            <el-tag  effect="plain" style="margin-right: 5px">{{ materialClassificationEnum.VL[item] }}</el-tag>
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
        <template v-slot="scope">
          <el-tag :type="scope.row.type === warehouseTypeEnum.NORMAL.V ? 'info' : 'warning'">{{
            warehouseTypeEnum.VL[scope.row.type]
          }}</el-tag>
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
        label="状态"
        align="center"
        width="100px"
      >
        <template v-slot="scope">
          <template v-if="useCheckPermission(permission.edit)">
            <el-switch :disabled="scope.row.enabledLoading" v-model="scope.row.enabled" class="drawer-switch" @change="handleEnabledChange(scope.row)" />
          </template>
          <template v-else>
            {{ enabledEnum.VL[scope.row.enabled] }}
          </template>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column v-permission="[...permission.edit, ...permission.del]" label="操作" width="130px" align="center" fixed="right">
        <template v-slot="scope">
          <udOperation :data="scope.row" :permission="permission" />
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
import { ref } from 'vue'
import EO from '@enum'
import { materialClassificationEnum } from '@enum-ms/classification'
import { warehouseTypeEnum } from '@enum-ms/wms'

import useCRUD from '@compos/use-crud'
import useCrudEnabledChange from '@compos/use-crud-enabled-change'
import useMaxHeight from '@compos/use-max-height'
import useCheckPermission from '@compos/use-check-permission'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'
import mBatchForm from './module/batch-form'

// crud交由presenter持有
const permission = {
  get: ['config_wms_warehouse:get'],
  add: ['config_wms_warehouse:add'],
  edit: ['config_wms_warehouse:edit'],
  del: ['config_wms_warehouse:del']
}

const optShow = {
  batchAdd: true,
  add: true,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()

const { maxHeight } = useMaxHeight()

const { CRUD, crud, columns } = useCRUD(
  {
    title: '仓库设置',
    formStore: true,
    formStoreKey: 'CONFIG_WMS_WAREHOUSE',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    queryOnPresenterCreated: false
  },
  tableRef
)

const { handleEnabledChange } = useCrudEnabledChange({ CRUD, crud, editEnabled })

CRUD.HOOK.handleRefresh = (crud, { data: { content }}) => {
  content.forEach(v => {
    const mt = v.materialType
    v.materialType = EO.getBits(materialClassificationEnum, mt, 'V')
  })
}
</script>
