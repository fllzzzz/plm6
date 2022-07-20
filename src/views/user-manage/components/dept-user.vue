<template>
  <common-dialog
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="人员配置"
    :center="false"
    :close-on-click-modal="false"
  >
  <template #titleRight>
    <common-button v-if="showType === 'edit'" v-loading="submitLoading" type="primary" size="mini" @click="submit">提交</common-button>
  </template>
  <div>
    <div class="header-container">
      <div class="filter-container">
        <div class="filter-left-box">
          <el-input v-model.trim="filterText" class="filter-item" placeholder="输入关键字进行过滤" style="width: 270px" />
        </div>
      </div>
    </div>
    <div class="user-tree tree-container" :loading="treeLoading">
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
  </div>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps, computed, watch, defineEmits, nextTick } from 'vue'
import { roleBindUser, saveRoleBindUser } from '@/api/user-manage/role'
import useUserDeptTree from '@compos/store/use-user-dept-tree'
import { isNotBlank } from '@data-type/index'
import { ElMessage } from 'element-plus'
import useVisible from '@compos/use-visible'

const props = defineProps({
  // permission: {
  //   type: Object,
  //   required: true
  // },
  showType: {
    type: String,
    default: undefined
  },
  roleId: {
    type: [Number, String]
  },
  refresh: {
    type: Boolean,
    default: false
  }
})
const { userDeptTree } = useUserDeptTree()
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const tree = ref()
const filterText = ref()
// const isEditing = ref(false)
const submitLoading = ref(false)
const defaultProps = {
  children: 'children',
  label: 'label'
}
const disabledUser = ref([])
const noDisabledUser = ref([])
const checkedList = ref([])
const treeLoading = ref(false)

watch(
  () => props.roleId,
  (val) => {
    if (val) {
      fetchUsers()
    } else {
      checkedList.value = []
      resetChecked()
    }
  },
  { deep: true, immediate: true }
)
watch(
  () => visible,
  (val) => {
    if (val) {
      fetchUsers()
    } else {
      checkedList.value = []
      resetChecked()
    }
  },
  { deep: true, immediate: true }
)

watch(
  userDeptTree,
  (list) => {
    if (isNotBlank(userDeptTree.value)) {
      fetchUserTree()
      fetchUsers()
    }
  },
  { immediate: true, deep: true }
)

// tree过滤输入监听
watch(filterText, (val) => {
  tree.value.filter(val)
})

const userList = computed(() => {
  return props.showType === 'detail' ? disabledUser.value : noDisabledUser.value
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
async function fetchUsers() {
  let userIds = []
  if (!props.roleId) {
    return
  }
  try {
    const data = await roleBindUser({ roleId: props.roleId }) || []
    userIds = data.map((v) => v.id)
  } catch (error) {
    console.log(error)
  } finally {
    checkedList.value = userIds
    resetChecked()
  }
}

async function submit() {
  try {
    submitLoading.value = true
    let checkedNodes = tree.value.getCheckedKeys(true)
    checkedNodes = checkedNodes.filter((v) => v > 0)
    await saveRoleBindUser({ roleId: props.roleId, userIds: checkedNodes })
    checkedList.value = checkedNodes
    ElMessage({
      message: '人员设置成功',
      type: 'success'
    })
    emit('success')
  } catch (error) {
    console.log('提交用户', error)
  } finally {
    submitLoading.value = false
  }
}
function resetChecked() {
  if (tree.value) {
    nextTick(() => {
      tree.value.setCheckedKeys(checkedList.value)
    })
  }
}
</script>

<style lang="scss" scoped>
.tree-container {
  height: 60vh;
  overflow-y: auto;
  box-shadow: inset -5px 3px 20px 0px rgba(195, 191, 191, 0.08);
  .filter-tree {
    height: 295px;
  }
}
::v-deep(.user-tree .el-checkbox__input.is-disabled.is-indeterminate .el-checkbox__inner) {
  background-color: #1890ff !important;
  border-color: #1890ff !important;
}
::v-deep(.user-tree .el-checkbox__input.is-disabled.is-checked .el-checkbox__inner) {
  background-color: #1890ff !important;
  border-color: #1890ff !important;
}
</style>
