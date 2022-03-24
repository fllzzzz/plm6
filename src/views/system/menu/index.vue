<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader ref="header" :permission="permission" />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
      lazy
      :load="load"
      :tree-props="{children: 'newChildren', hasChildren: 'hasChildren'}"
      row-key="id"
      @selection-change="crud.selectionChangeHandler"
      return-source-data
      :showEmptySymbol="false"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        :show-overflow-tooltip="true"
        label="菜单名称"
        min-width="280px"
      />
      <el-table-column v-if="columns.visible('type')" key="type" prop="type" :show-overflow-tooltip="true" label="菜单类型" width="120px">
        <template v-slot="scope">
          {{ systemMenusTypeEnum.VL[scope.row.type] }}
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('icon')" key="icon" prop="icon" label="图标" align="center" width="60px">
        <template v-slot="scope">
          <svg-icon :icon-class="scope.row.icon ? scope.row.icon : ''" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('sort')" key="sort" prop="sort" label="排序" align="center">
        <template v-slot="scope">
          {{ scope.row.sort }}
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('path')" key="path" prop="path" :show-overflow-tooltip="true" label="路由地址" />
      <el-table-column
        v-if="columns.visible('permission')"
        key="permission"
        prop="permission"
        :show-overflow-tooltip="true"
        label="权限标识"
      />
      <el-table-column
        v-if="columns.visible('component')"
        key="component"
        prop="component"
        :show-overflow-tooltip="true"
        label="组件路径"
      />
      <el-table-column v-if="columns.visible('iframe')" key="iframe" prop="iframe" label="外链" align="center" width="75px">
        <template v-slot="scope">
          <span v-if="scope.row.iframe">是</span>
          <span v-else>否</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('cache')" key="cache" prop="cache" label="缓存" align="center" width="75px">
        <template v-slot="scope">
          <span v-if="scope.row.cache">是</span>
          <span v-else>否</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('hidden')" key="hidden" prop="hidden" label="可见" align="center" width="75px">
        <template v-slot="scope">
          <span v-if="scope.row.hidden">否</span>
          <span v-else>是</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('category')" key="category" prop="category" label="类别" align="center" width="75px">
        <template v-slot="scope">
          {{ systemMenusCategoryEnum.VL[scope.row.category] }}
        </template>
      </el-table-column>
      <!-- <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建日期" width="135px">
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.createTime) }}</span>
        </template>
      </el-table-column> -->
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.edit, ...permission.del])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <udOperation :data="scope.row" :permission="permission" del-prompt="确定删除吗,如果存在下级节点则一并删除，此操作不能撤销！" />
        </template>
      </el-table-column>
    </common-table>
    <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/system/menu'
import { ref, nextTick } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import { systemMenusTypeEnum, systemMenusCategoryEnum } from '@enum-ms/system'
// import { parseTime } from '@/utils/date'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'

// crud交由presenter持有
const permission = {
  get: ['menu:get'],
  add: ['menu:add'],
  edit: ['menu:edit'],
  del: ['menu:del']
}

const tableRef = ref()
// const allData = ref([])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '菜单',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.menu',
  paginate: true,
  extraHeight: 157
})

// 获取所有数据
function getAllData(data, pid) {
  data.map(v => {
    v.parentArray = [...pid]
    if (v.pid) {
      v.parentArray.push(v.pid)
    }
    if (v.children && v.children.length > 0) {
      getAllData(v.children, v.parentArray)
    }
  })
}
CRUD.HOOK.handleRefresh = (crud, data) => {
  getAllData(data.data.content, [])
  data.data.content = data.data.content.map(v => {
    v.hasChildren = !!v.children
    return v
  })
}

function load({ row, treeNode, resolve }) {
  const newChildren = row.children.map(v => {
    v.hasChildren = !!v.children
    if (v.hasChildren) {
      v.newChildren = JSON.parse(JSON.stringify(v.children))
      delete v.newChildren.children
    }
    return v
  })
  resolve([...newChildren])
  nextTick(() => {
    tableRef.value.expandParent(row, true)
  })
}
</script>
