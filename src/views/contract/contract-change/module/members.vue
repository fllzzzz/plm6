<template>
  <div>
    <div class="user-tree tree-container" :loading="treeLoading">
      <template v-if="!judgeSameValue(originContractInfo?.relationUserId,detail?.relationUserId)">
        <div style="font-size:13px;color:#666;">修改前：<span>{{originContractInfo?.relationUserName}}</span></div>
        <div style="font-size:13px;color:red;">修改后：<span>{{detail?.relationUserName}}</span></div>
      </template>
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
</template>

<script setup>
import { ref, defineProps, computed, watch } from 'vue'
import useUserDeptTree from '@compos/store/use-user-dept-tree'
import { isNotBlank } from '@data-type/index'
import { judgeSameValue } from '@/views/contract/info/judgeSameValue'

const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  isModify: {
    type: Boolean,
    default: false
  },
  checkedList: {
    type: Array,
    default: () => []
  },
  detail: {
    type: Object,
    default: () => {}
  },
  originContractInfo: {
    type: Object,
    default: () => {}
  }
})
const { userDeptTree } = useUserDeptTree()

const tree = ref()
const defaultProps = {
  children: 'children',
  label: 'label'
}
const disabledUser = ref([])
const noDisabledUser = ref([])
const treeLoading = ref(false)

watch(
  () => props.checkedList,
  (val) => {
    if (val.length > 0) {
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
    }
  },
  { immediate: true, deep: true }
)

const userList = computed(() => {
  return props.isModify ? noDisabledUser.value : disabledUser.value
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
  tree.value.setCheckedKeys(props.checkedList)
}

</script>

<style lang="scss" scoped>
.tree-container {
    height: 60vh;
    overflow-y: auto;
    box-shadow: inset -5px 3px 20px 0px rgba(195, 191, 191, 0.08);
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
