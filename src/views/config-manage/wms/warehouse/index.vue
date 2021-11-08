<template>
  <div class="app-container warehouse-config">
      <!--工具栏-->
      <mHeader />
      <!-- 表格渲染 -->
      <el-table ref="table" v-loading="crud.loading" :data="crud.data" style="width: 100%;" @selection-change="crud.selectionChangeHandler">
        <el-table-column type="selection" width="55" align="center" />
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column v-if="columns.visible('name')" key="name" :show-overflow-tooltip="true" prop="name" label="仓库位置" align="left" min-width="150" />
        <el-table-column v-if="columns.visible('materialType')" key="materialType" :show-overflow-tooltip="true" prop="name" label="可存储材料类型" align="left" min-width="300">
          <template v-slot="scope">
            <template v-for="(item, index) in materialClassificationEnum">
              <el-tag v-if="scope.row.materialType & item.V" :key="index" effect="plain" style="margin-right:5px">{{ item.L }}</el-tag>
            </template>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('type')" key="type" :show-overflow-tooltip="true" prop="name" label="仓库类型" align="center" width="100px">
          <template v-slot="scope">
            <el-tag :type="scope.row.type === warehouseTypeEnum.NORMAL.V ? 'info' : 'warning'">{{ warehouseTypeEnum.VL[scope.row.type] }}</el-tag>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('sort')" key="sort" :show-overflow-tooltip="true" prop="sort" label="排序" align="center" width="100px" />
        <el-table-column v-if="columns.visible('state')" :show-overflow-tooltip="true" prop="state" label="状态" align="center" width="100px">
          <template v-slot="scope">
            <template v-if="scope.$index == 0">
              {{ scope.row.state ? '使用中':'禁用中' }}
            </template>
            <template v-if="scope.$index != 0">
              <el-switch v-model="scope.row.state" class="drawer-switch" />
            </template>
          </template>
        </el-table-column>
        <!--编辑与删除-->
        <el-table-column
          v-permission="[...permission.edit, ...permission.del]"
          label="操作"
          width="130px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <udOperation
              :data="scope.row"
              :permission="permission"
            />
          </template>
        </el-table-column>
      </el-table>
    <!-- 表单 -->
    <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/config-manage/wms/warehouse'
import useCRUD from '@compos/use-crud'
import { materialClassificationEnum } from '@enum-ms/classification'
import { warehouseTypeEnum } from '@enum-ms/wms'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'

// crud交由presenter持有
const permission = {
  get: ['configWmsWarehouseLocal:get'],
  add: ['configWmsWarehouseLocal:add'],
  edit: ['configWmsWarehouseLocal:edit'],
  del: ['configWmsWarehouseLocal:del']
}

const { crud, columns } = useCRUD({
  title: '仓库位设置',
  permission: { ...permission },
  crudApi: { ...crudApi },
  queryOnPresenterCreated: false
})

</script>
