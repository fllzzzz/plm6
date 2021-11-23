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
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="80px" :inline="true">
       <el-form-item label="名称" prop="name">
        <el-input v-model="form.name" style="width: 370px;" />
      </el-form-item>
      <el-form-item label="上级部门" prop="pid" v-if="form.id!=1">
        <el-popover
          placement="bottom-start"
          width="450"
          trigger="click"
          v-model:visible="menuVisible"
          ref="menuPopover"
        >
          <menuSelect ref="menuSelectRef" @selected="menuSelected" style="width:400px;" :pid="form.pid" :defaultProps="{ children: 'children', label: 'name' }" :treeMenu="deptTreeData"/>
          <template #reference>
            <el-input v-model="form.pName" style="width: 450px;" placeholder="点击选择上级类目" readonly />
          </template>
        </el-popover>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'
import { regForm } from '@compos/use-crud'
import menuSelect from '@/components-system/system/tree-select/index.vue'
import { deptTree } from '@/api/system/member-manage/dept'
import { isNotBlank } from '@data-type/index'

const formRef = ref()
const menuPopover = ref()
const menuSelectRef = ref()
const menuVisible = ref(false)
const deptTreeData = ref([])
const defaultForm = {
  id: undefined,
  name: undefined,
  pid: 1,
  pName: undefined
}
const { CRUD, crud, form } = regForm(defaultForm, formRef)

const rules = {
  name: [{ required: true, message: '请填写部门称', trigger: 'blur' }]
}

function menuSelected(value) {
  form.pid = value.id
  form.pName = value.name
  menuVisible.value = false
}

// 加载数据
fetchDeptTree()

// 拉取部门树
async function fetchDeptTree() {
  try {
    deptTreeData.value = await deptTree({ 'hasRoot': true })
  } catch (error) {
    console.log('部门tree', error)
  }
}

let allMenu = []
function arrFn(source) {
  source.forEach(el => {
    allMenu.push(el)
    el.children && el.children.length > 0 ? arrFn(el.children) : ''
  })
}
// 打开编辑弹窗前做的操作
CRUD.HOOK.afterToEdit = (crud, form) => {
  if (isNotBlank(form.pid)) {
    allMenu = []
    arrFn(deptTreeData.value)
    const value = allMenu.find(v => v.id === form.pid)
    form.pName = isNotBlank(value) ? value.name : ''
  }
}
</script>
