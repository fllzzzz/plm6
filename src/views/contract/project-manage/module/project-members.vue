<template>
  <el-form
    ref="memberRef"
    :model="form"
    :rules="rules"
    inline
    class="from-text"
    size="small"
    label-position="right"
    label-width="110px"
  >
    <el-form-item label="成员模板">
      <common-select
        v-model="templateName"
        :options="options"
        :disabled="options.isDefault"
        :dataStructure="{key: 'id', label: 'templateName', value: 'id'}"
        type="other"
        placeholder="成员模板"
        class="input-underline"
        style="width:200px"
        @change="userIdListChange"
      />
      <div class="user-tree tree-container">
        <el-tree
          ref="tree"
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
      </div>
    </el-form-item>
  </el-form>
</template>
<script setup>
import { ref, watch, computed, defineEmits, defineExpose } from 'vue'
import useUserDeptTree from '@compos/store/use-user-dept-tree'
import { isNotBlank } from '@data-type/index'
import { get, getUserByTemplate } from '@/api/contract/project-member-template'

const emit = defineEmits(['getUserIds'])
const { userDeptTree } = useUserDeptTree()
const memberRef = ref()
const tree = ref()
const isEditing = ref(false)
const defaultProps = {
  children: 'children',
  label: 'label'
}
const disabledUser = ref([])
const noDisabledUser = ref([])
const checkedList = ref([])
const templateName = ref()
const options = ref([])
const rules = ref({})
const form = ref({})
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
  return isEditing.value ? noDisabledUser.value : disabledUser.value
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
  tree.forEach(node => {
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

function resetChecked() {
  tree.value.setCheckedKeys(checkedList.value)
}

function userIdListChange(val) {
  fetchMembers(val)
}
// 获取用户详情
async function fetchMembers(id) {
  let userIds = []
  try {
    const { content = [] } = await getUserByTemplate({ id: id })
    userIds = content.map(v => v.id)
  } catch (error) {
    console.log(error)
  } finally {
    checkedList.value = userIds
    emit('getUserIds', userIds)
    resetChecked()
  }
}

const checkUser = ref([])

function getCheckUser() {
  let checkedNodes = tree.value.getCheckedKeys(true)
  checkedNodes = checkedNodes.filter(v => v > 0)
  checkUser.value = checkedNodes
}

getUserTemplate()

async function getUserTemplate() {
  let list = []
  try {
    const { content = [] } = await get()
    list = content.filter(v => v.status === 1)
    list.forEach(i => {
      if (i.isDefault === true) {
        templateName.value = i.id
        fetchMembers(i.id)
      }
    })
  } catch (error) {
    console.log(error)
  } finally {
    options.value = list
  }
}

async function validateForm() {
  try {
    const valid = await memberRef.value.validate()
    return valid
  } catch (error) {
    console.log('error', error)
    return false
  }
}

defineExpose({
  validateForm,
  getCheckUser,
  checkUser
})
</script>

<style lang="scss" scoped>
.user-tree{
  margin-top: 25px;
  width: 400px;
}
.tree-container {
    height: 75vh;
    overflow-y: auto;
    .filter-tree {
        height:295px;
    }

}
::v-deep(.user-tree .el-checkbox__input.is-disabled.is-indeterminate .el-checkbox__inner){
    background-color: #1890ff!important;
    border-color: #1890ff!important;
}
::v-deep(.user-tree .el-checkbox__input.is-disabled.is-checked .el-checkbox__inner){
    background-color: #1890ff!important;
    border-color: #1890ff!important;
}
</style>
