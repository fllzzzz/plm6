<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="600px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="80px" :inline="true" v-loading="loading.data">
      <el-form-item label="类型" prop="type">
        <common-radio-button
          v-model="form.type"
          :options="systemMenusTypeEnum.ENUM"
          type="enum"
          size="mini"
          style="width: 178px;margin-right:178px;"
        />
      </el-form-item>
      <el-form-item v-show="form.type === systemMenusTypeEnum.MODULE.V" label="类别" prop="category">
        <common-radio-button
          v-model="form.category"
          :options="systemMenusCategoryEnum.ENUM"
          type="enum"
          size="mini"
          style="width: 178px"
        />
      </el-form-item>
      <el-form-item v-show="form.type !== systemMenusTypeEnum.BUTTON.V" label="菜单标题" prop="name">
        <el-input v-model="form.name" :style="form.type === systemMenusTypeEnum.MODULE.V ? 'width: 450px' : 'width: 178px'" placeholder="菜单标题" />
      </el-form-item>
      <el-form-item v-show="form.type === systemMenusTypeEnum.BUTTON.V" label="按钮名称" prop="name">
        <el-input v-model="form.name" placeholder="按钮名称" style="width: 178px;" />
      </el-form-item>
      <el-form-item v-show="form.type !== systemMenusTypeEnum.MODULE.V" label="权限标识" prop="permission">
        <el-input v-model="form.permission" :disabled="form.iframe" placeholder="权限标识" style="width: 450px;" />
      </el-form-item>
      <el-form-item v-show="form.type !== systemMenusTypeEnum.BUTTON.V" label="菜单图标" prop="icon">
        <el-popover
          placement="bottom-start"
          width="450"
          trigger="click"
          v-model:visible="visible"
          ref="iconPopover"
        >
          <IconSelect ref="iconSelectRef" @selected="selected" style="width:400px;"/>
          <template #reference>
            <el-input v-model="form.icon" style="width: 450px;" placeholder="点击选择图标" readonly>
              <template #prefix>
                <svg-icon v-if="form.icon" :icon-class="form.icon" class="el-input__icon" style="height: 32px;width: 16px;" />
                <i v-else class="el-icon-search el-input__icon" />
              </template>
            </el-input>
          </template>
        </el-popover>
      </el-form-item>
      <el-form-item v-if="form.type === systemMenusTypeEnum.MENU.V" label="路由地址" prop="path">
        <el-input v-model="form.path" placeholder="路由地址" style="width: 450px;" />
      </el-form-item>
      <el-form-item v-if="form.type !== systemMenusTypeEnum.BUTTON.V" label="转发地址" prop="path">
        <el-input v-model="form.redirect" placeholder="转发地址" style="width: 450px;" />
      </el-form-item>
      <el-form-item v-show="!form.iframe && form.type === systemMenusTypeEnum.MENU.V" label="组件路径" prop="component">
        <el-input v-model="form.component" style="width: 450px;" placeholder="组件路径" />
      </el-form-item>
      <el-form-item v-show="!form.iframe && form.type === systemMenusTypeEnum.MENU.V" label="组件名称" prop="componentName">
        <el-input v-model="form.componentName" style="width: 178px;" placeholder="匹配组件内Name字段" />
      </el-form-item>
      <el-form-item v-show="form.type !== systemMenusTypeEnum.BUTTON.V" label="外链菜单" prop="iframe">
        <el-radio-group v-model="form.iframe" size="mini">
          <el-radio-button :label="true">是</el-radio-button>
          <el-radio-button :label="false">否</el-radio-button>
        </el-radio-group>
      </el-form-item>
      <el-form-item v-show="form.type === systemMenusTypeEnum.MENU.V" label="菜单缓存" prop="cache" style="min-width: 258px;">
        <el-radio-group v-model="form.cache" size="mini">
          <el-radio-button :label="true">是</el-radio-button>
          <el-radio-button :label="false">否</el-radio-button>
        </el-radio-group>
      </el-form-item>
      <el-form-item v-show="form.type !== systemMenusTypeEnum.BUTTON.V" label="菜单可见" prop="hidden">
        <el-radio-group v-model="form.hidden" size="mini">
          <el-radio-button :label="false">是</el-radio-button>
          <el-radio-button :label="true">否</el-radio-button>
        </el-radio-group>
      </el-form-item>
      <el-form-item v-show="form.type === systemMenusTypeEnum.MENU.V" label="单个子菜单可见" prop="alwaysShow" label-width="120px" style="min-width: 257px;">
        <el-radio-group v-model="form.alwaysShow" size="mini" style="width: 100px;">
          <el-radio-button :label="true">是</el-radio-button>
          <el-radio-button :label="false">否</el-radio-button>
        </el-radio-group>
      </el-form-item>
      <el-form-item v-if="form.type === systemMenusTypeEnum.BUTTON.V" label="权限类型" prop="type">
        <el-select v-model="form.permissionTypeId" placeholder="请选择权限类型" style="width: 178px;">
          <el-option
            v-for="item in permissionTypes"
            :key="item.id"
            :label="item.name"
            :value="item.id"
          />
        </el-select>
      </el-form-item>
      <el-form-item label="菜单排序" prop="sort">
        <el-input-number v-model.number="form.sort" :min="1" :max="999" controls-position="right" style="width: 178px;" />
      </el-form-item>
      <el-form-item label="上级类目" prop="pid">
        <el-popover
          placement="top-start"
          width="450"
          trigger="click"
          v-model:visible="menuVisible"
          ref="menuPopover"
        >
          <menuSelect ref="menuSelectRef" @selected="menuSelected" style="width:400px;" :pid="form.pid" :treeMenu="menuTreeData" :defaultProps="{ children: 'children', label: 'label' }"/>
          <template #reference>
            <el-input v-model="form.pName" style="width: 450px;" placeholder="点击选择上级类目" readonly />
          </template>
        </el-popover>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { reactive, ref } from 'vue'
import { regForm } from '@compos/use-crud'
import { menuTree } from '@/api/system/menu'
import { systemMenusTypeEnum, systemMenusCategoryEnum } from '@enum-ms/system'
import { permissionTypeAll } from '@/api/system/permission-type'
import menuSelect from '@/components-system/common/tree-select.vue'
import IconSelect from '@comp/iconSelect/index.vue'
import { isNotBlank } from '@data-type/index'

const formRef = ref()
const iconPopover = ref()
const menuPopover = ref()
const menuSelectRef = ref()
const iconSelectRef = ref()
const visible = ref(false)
const menuVisible = ref(false)
const permissionTypes = ref([])
const menuTreeData = ref([])
const choseMenu = ref({})
const loading = reactive({
  // 加载
  data: false
})
const defaultForm = {
  id: undefined,
  name: undefined,
  sort: 1,
  path: undefined,
  component: undefined,
  componentName: undefined,
  iframe: false,
  roles: [],
  pid: 0,
  pName: '顶级类目',
  icon: null,
  cache: false,
  hidden: false,
  type: 0,
  category: systemMenusCategoryEnum.PC.V,
  permission: null,
  alwaysShow: false
}
const { CRUD, crud, form } = regForm(defaultForm, formRef)

const rules = {
  name: [{ required: true, message: '请填写名称', trigger: 'blur' }]
}
function selected(name) {
  form.icon = name
  visible.value = false
}

function menuSelected(value) {
  choseMenu.value = value
  form.pid = value.id
  form.pName = value.label
  form.category = value.category
  menuVisible.value = false
}

getPermissionType()

async function getPermissionType() {
  try {
    const { content } = await permissionTypeAll()
    permissionTypes.value = content
  } catch (e) {
    console.log('权限类型', e)
  }
}
let allMenu = []
function arrFn(source) {
  source.forEach(el => {
    allMenu.push(el)
    el.children && el.children.length > 0 ? arrFn(el.children) : ''
  })
}

fetchMenuTree()
// 菜单树
async function fetchMenuTree() {
  try {
    loading.data = true
    const menu = { id: 0, label: '顶级类目', children: [] }
    menu.children = await menuTree()
    menuTreeData.value = [menu]
  } catch (error) {
    console.log('菜单树', error)
  } finally {
    loading.data = false
  }
}

CRUD.HOOK.afterToEdit = (crud, form) => {
  if (isNotBlank(form.pid)) {
    allMenu = []
    arrFn(menuTreeData.value)
    const value = allMenu.find(v => v.id === form.pid)
    form.pName = value.label
    form.category = value.category
  }
}
</script>
