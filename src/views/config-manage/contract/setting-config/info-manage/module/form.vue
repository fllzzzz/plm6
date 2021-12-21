<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="860px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
        <el-form-item label="模板名称" prop="templateName">
          <el-input v-model="form.templateName" type="text" placeholder="模板名称" style="width: 250px" />
        </el-form-item>
        <el-form-item label="描述信息" prop="remark">
          <el-input
            v-model="form.remark"
            type="textarea"
            :autosize="{ minRows: 6, maxRows: 8 }"
            :maxLength="500"
            placeholder="描述信息"
            style="max-width: 500px"
          />
        </el-form-item>
        <el-form-item label="默认模板" prop="isDefault">
          <el-checkbox v-model="form.isDefault" />
        </el-form-item>
        <el-form-item label="用户信息" prop="isDefault">
          <el-input v-model="filterText" class="filter-item" placeholder="输入关键字进行过滤" style="width: 270px" />
          <el-tree
            ref="treeRef"
            class="filter-tree"
            :data="userList"
            :empty-text="'没有用户'"
            :props="defaultProps"
            default-expand-all
            show-checkbox
            node-key="id"
            :filter-node-method="filterNode"
            :default-checked-keys="checkedList"
          />
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, computed, watch } from 'vue'
import { regForm } from '@compos/use-crud'
import { getUserAllSimpleByProject as getAllUser } from '@/api/contract/member-config'
import useUserDeptTree from '@compos/store/use-user-dept-tree'
import { isNotBlank } from '@data-type/index'
import { systemEnabledEnum } from '@enum-ms/system'

const { loaded, userDeptTree } = useUserDeptTree()
const formRef = ref()
const defaultForm = {
  id: undefined,
  templateName: undefined,
  remark: undefined,
  isDefault: false,
  userIdList: [],
  status: systemEnabledEnum.ENUM.TRUE.V,
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const rules = {
  templateName: [{ required: true, message: '请输入模板名称', trigger: 'blur' }],
  remark: [{ required: true, message: '请输入描述信息', trigger: 'change' }],
}

const treeRef = ref()
const filterText = ref()
const isEditing = ref(false)
const submitLoading = ref(false)
const defaultProps = {
  children: 'children',
  label: 'label',
}
const disabledUser = ref([])
const noDisabledUser = ref([])
const checkedList = ref([])
const treeLoading = ref(false)
// const userList = ref([])

watch(
  () => filterText.value,
  (val) => {
    if (val) {
      treeRef.value.filter(val)
    }
  }
)

watch(
  () => crud.form.id,
  (val) => {
    if (val) {
      fetchMembers(val)
    } else {
      checkedList.value = []
      if (treeRef.value) {
        resetChecked()
      }
    }
  },
  { deep: true, immediate: true }
)

watch(
  userDeptTree,
  (list) => {
    if (isNotBlank(userDeptTree.value)) {
      fetchUserTree()
    }
  },
  { immediate: true, deep: true }
)

const userList = computed(() => {
  return noDisabledUser.value
})

function filterNode(value, data) {
  if (!value) return true
  return data.label.indexOf(value) !== -1 || (data.parentLabel && data.parentLabel.indexOf(value) !== -1)
}
function fetchUserTree() {
  let disabledUserData = []
  let noDisabledUserData = []
  disabledUserData = JSON.parse(JSON.stringify(userDeptTree.value))
  traversalTree(disabledUserData, true)
  noDisabledUserData = JSON.parse(JSON.stringify(userDeptTree.value))
  traversalTree(noDisabledUserData, false)
  disabledUser.value = disabledUserData
  noDisabledUser.value = noDisabledUserData
}

function traversalTree(tree, disabled, parentLabel) {
  tree.forEach((node) => {
    node.disabled = disabled
    node.parentLabel = parentLabel
    if (!node.isUser) {
      if (node.id === 1) {
        node.label = '<公司>' + node.label
      } else {
        node.label = '<部门>' + node.label
      }
      node.id = -node.id
    } else {
      // TODO: 同名
    }
    // TODO: 可能需要对为空的部门做处理
    if (node.children) {
      traversalTree(node.children, disabled, node.label)
    }
  })
}
async function fetchMembers(id) {
  let userIds = []
  try {
    const { content } = await getAllUser({ id: id })
    userIds = content.map((v) => v.id)
  } catch (error) {
    console.log(error)
  } finally {
    checkedList.value = userIds
    resetChecked()
  }
}
function cancelEdit() {
  isEditing.value = false
  resetChecked()
}

function getUser() {
  let checkedNodes = treeRef.value.getCheckedKeys(true)
  checkedNodes = checkedNodes.filter((v) => v > 0)
  checkedList.value = checkedNodes
}

function resetChecked() {
  treeRef.value.setCheckedKeys(checkedList.value)
}

CRUD.HOOK.beforeSubmit = (crud, form) => {
  getUser()
  crud.form.userIdList = checkedList.value || []
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
