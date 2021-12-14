<template>
  <div>
    <div class="header-container">
      <div class="filter-container">
        <div class="filter-left-box">
          <el-input
            v-model="filterText"
            class="filter-item"
            placeholder="输入关键字进行过滤"
            style="width:270px"
          />
        </div>
        <div class="filter-right-box" style="display:inline-block;">
          <common-button
            v-if="!isEditing"
            class="filter-item"
            size="small"
            type="primary"
            icon="el-icon-edit"
            @click="isEditing = true"
          >修改</common-button>
          <div v-else style="display:inline-block">
            <common-button
              class="filter-item"
              size="small"
              type="warning"
              icon="el-icon-refresh"
              @click="cancelEdit"
            >取消</common-button>
            <common-button
              :loading="submitLoading"
              class="filter-item"
              size="small"
              style="margin-left:0px;"
              type="success"
              icon="el-icon-circle-plus-outline"
              @click="submit"
            >保存</common-button>
          </div>
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
</template>

<script setup>
import { ref, defineProps, computed, watch } from 'vue'
// import { getUserTree } from '@/api/common'
import { getUserAllSimpleByProject as getAllUser } from '@/api/contract/project'
import useUserDeptTree from '@compos/store/use-user-dept-tree'
import { isNotBlank } from '@data-type/index'
// import { editUsers } from '@/api/contract/project'

const props=defineProps({
  permission: {
    type: Object,
    required: true
  },
  projectId: {
    type: [Number, String]
  },
  refresh: {
    type: Boolean,
    default: false
  }
})
const { loaded, userDeptTree } = useUserDeptTree()

const tree=ref()
const filterText = ref()
const isEditing = ref(false)
const submitLoading = ref(false)
const defaultProps = {
  children: 'children',
  label: 'label'
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
      tree.value.filter(val)
    }
  }
)

// watch(
//   () => props.refresh,
//   (val) => {
//     if (val) {
//       fetchMembers()
//     } else {
//       checkedList.value = []
//       resetChecked()
//     }
//   },
//   { deep: true, immediate: true }
// )

watch(
  userDeptTree,
  (list) => {
    if(isNotBlank(userDeptTree.value)){
      fetchUserTree()
      fetchMembers()
    }
  },
  { immediate: true, deep: true }
)

const userList = computed(()=>{
  return isEditing.value? noDisabledUser.value: disabledUser.value
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
async function fetchMembers() {
  let userIds = []
  try {
    const { content } = await getAllUser(props.projectId)
    userIds = content.map(v => v.id)
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
async function submit() {
  try {
    submitLoading.value = true
    let checkedNodes = tree.value.getCheckedKeys(true)
    checkedNodes = checkedNodes.filter(v => v > 0)
    // await editUsers({ projectId: props.projectId, ids: checkedNodes })
    checkedList.value = checkedNodes
    // this.$message({
    //   message: '更新项目成员成功',
    //   type: 'success'
    // })
    isEditing.value = false
  } catch (error) {
    console.log('提交用户', error)
  } finally {
    submitLoading.value = false
  }
}
function resetChecked() {
  tree.value.setCheckedKeys(checkedList.value)
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
