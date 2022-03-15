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
      row-key="id"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" label="名称" />
      <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" :show-overflow-tooltip="true" label="描述" />
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.edit, ...permission.del])"
        label="操作"
        width="180px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <common-button size="mini" type="info" icon="el-icon-key" @click="showMenuAssignation(scope.row)" />
          <udOperation :data="scope.row" :permission="permission"/>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
    <common-drawer
      v-model="menuVisible"
      :with-header="true"
      direction="rtl"
      size="100%"
      :show-close="false"
    >
      <template #title>
        <div class="dialog-title">
          <div class="title">
            <span>菜单分配</span>
          </div>
          <div class="tip">
            <el-tag v-if="currentRow.name">{{ currentRow.name }}</el-tag>
          </div>
          <div style="margin:10px 10px;">
            <el-input
              v-model="menuQuery"
              placeholder="可搜索菜单"
              size="small"
              style="width:300px;margin-right:10px;"
              @keyup.enter="searchMenu"
            />
            <common-button size="mini" type="success" icon="el-icon-search" :loading="searchLoading" @click.stop="searchMenu">搜索</common-button>
            <common-button size="mini" type="warning" icon="el-icon-refresh-left" :loading="resetLoading" @click.stop="resetMenu">重置</common-button>
          </div>
        </div>
        <span style="float:right;">
          <common-button type="primary" size="small" :loading="menuLoading" @click="saveMenu">保存</common-button>
          <common-button type="info" size="small" @click="closeMenuAssignation">取消</common-button>
        </span>
      </template>
      <template #content>
        <div class="dialog-container">
          <menu-assignation
            v-if="menuVisible"
            ref="menu"
            :current-id="currentId"
            :menus="menus"
            :menu-ids="menuIds"
            :role-name="currentRow.name"
            :permission="permission"
            @updateSelect="updateSelect"
          />
        </div>
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import crudApi, { bindMenu } from '@/api/user-manage/role'
import { menuTree } from '@/api/system/menu'
import { roleConfigPM as permission } from '@/page-permission/user'

import { reactive, ref } from 'vue'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'
import MenuAssignation from './module/menu-assignation'
import pagination from '@crud/Pagination'

const tableRef = ref()
const menuVisible = ref(false)
const menuQuery = ref()
const searchLoading = ref(false)
const resetLoading = ref(false)
const menuLoading = ref(false)
const menus = ref([])
const originalMenus = ref([])
const selectMenus = ref([])
const currentId = ref()
const menuIds = ref()
let currentRow = reactive({})
const { crud, columns, CRUD } = useCRUD(
  {
    title: '角色',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.job',
  paginate: true,
  extraHeight: 157
})

getMenus()
async function getMenus() {
  try {
    const res = await menuTree()
    menus.value = res
    originalMenus.value = res
  } catch (error) {
    console.log('菜单树', error)
  }
}
// 关闭权限菜单
function closeMenuAssignation() {
  menuIds.value = []
  selectMenus.value = []
  menuVisible.value = false
}
// 搜索菜单
function searchMenu() {
  searchLoading.value = true
  const _menus = searchTreeNode(originalMenus.value, 'children', 'label', menuQuery.value)
  menus.value = JSON.parse(JSON.stringify(_menus))
  searchLoading.value = false
}
function searchTreeNode(tree, childField, field, value, pollingFloor = 1) {
  let treeCopy
  if (pollingFloor === 1) {
    treeCopy = JSON.parse(JSON.stringify(tree))
  } else {
    treeCopy = tree
  }
  return treeCopy.filter(n => {
    if (n[field].indexOf(value) !== -1) {
      return true
    } else if (n[childField] && n[childField].length) {
      const _arr = searchTreeNode(n[childField], childField, field, value, ++pollingFloor)
      if (_arr && _arr.length > 0) {
        n[childField] = _arr
        return true
      } else {
        return false
      }
    } else {
      return false
    }
  })
}
// 重置搜索菜单
function resetMenu() {
  resetLoading.value = true
  menuQuery.value = ''
  menus.value = JSON.parse(JSON.stringify(originalMenus.value))
  setTimeout(() => {
    resetLoading.value = false
  }, 500)
}
function updateSelect(menus) {
  selectMenus.value = menus
}
async function saveMenu() {
  menuLoading.value = true
  const role = { id: currentId.value, menus: selectMenus.value }
  try {
    await bindMenu(role)
    crud.notify('保存成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
    menuLoading.value = false
    menuVisible.value = false
    crud.toQuery()
  } catch (e) {
    menuLoading.value = false
    console.log('绑定菜单', e)
  }
}
// 打开权限菜单
function showMenuAssignation(row) {
  if (row) {
    menuVisible.value = true
    currentId.value = row.id
    currentRow = row
    menuIds.value = row.menus ? JSON.parse(JSON.stringify(row.menus)) : []
    selectMenus.value = row.menus ? JSON.parse(JSON.stringify(row.menus)) : []
    menuVisible.value = true
  }
}
</script>