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
      <el-table-column v-if="columns.visible('dept')" key="dept" prop="dept" label="所属部门">
        <template v-slot="scope">
          <div>{{ scope.row.deptName }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('sort')" key="sort" prop="sort" label="排序" align="center">
        <template v-slot="scope">
          {{ scope.row.sort }}
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('enabled')" key="enabled" prop="enabled" label="状态" align="center">
        <template v-slot="scope">
          <el-switch
            v-model="scope.row.enabled"
            active-color="#409EFF"
            inactive-color="#F56C6C"
            :active-value="systemEnabledEnum.TRUE.V"
            :inactive-value="systemEnabledEnum.FALSE.V"
            :disabled="!checkPermission(permission.edit)"
            @change="changeStatus(scope.row, scope.row.enabled)"
          />
        </template>
      </el-table-column>
      <el-table-column label="人员设置" align="center">
        <template v-slot="scope">
          <common-button
            v-if="checkPermission(permission.edit)"
            size="mini"
            icon="el-icon-edit"
            type="primary"
            @click="getDeptUser(scope.row);"
          />
          <common-button
            size="mini"
            icon="el-icon-view"
            type="primary"
            @click="getJobBindUser(scope.row)"
          />
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.edit, ...permission.del])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <udOperation :data="scope.row" :permission="permission"/>
        </template>
      </el-table-column>
    </common-table>
    <common-dialog
      append-to-body
      v-model="userVisible"
      top="10vh"
      width="480px"
      title="人员配置"
      :center="false"
      :before-close="handleClose"
      :close-on-click-modal="false"
    >
    <template #titleRight>
      <common-button v-loading="submitLoading" type="primary" size="mini" @click="userSubmit">提交</common-button>
    </template>
      <div>
        <div class="user-tree tree-container">
          <el-tree
            ref="tree"
            class="filter-tree"
            :data="userList"
            :empty-text="'没有用户'"
            :props="defaultProps"
            default-expand-all
            show-checkbox
            node-key="userId"
            :default-checked-keys="checkedList"
          />
        </div>
    </div>
    </common-dialog>
    <common-dialog
      append-to-body
      v-model="bindUserVisible"
      top="10vh"
      width="480px"
      title="已配置人员"
      :center="false"
      :before-close="closeBind"
      :close-on-click-modal="false"
    >
      <div>
        <div style="display:flex;">
          <span class="el-form-item__label" style="text-align:right;width:90px;line-height:36px;">部门:</span>
          <span class="el-form-item__content" style="line-height:36px;">{{bindUserList.deptName}}</span>
        </div>
        <div style="display:flex;">
          <span class="el-form-item__label" style="text-align:right;width:90px;line-height:36px;">岗位:</span>
          <span class="el-form-item__content" style="line-height:36px;">{{bindUserList.jobName}}</span>
        </div>
        <div style="display:flex;">
          <span class="el-form-item__label" style="text-align:right;width:90px;line-height:36px;">人员:</span>
          <template v-if="bindUserList?.userList?.length>0">
            <el-row class="el-form-item__content" style="line-height:36px;">
              <el-col :span="5" v-for="item in bindUserList.userList" :key="item.userId" ><span>{{item.userName}}</span></el-col>
            </el-row>
          </template>
          <template v-else><span class="el-form-item__content" style="line-height:36px;">-</span></template>

        </div>
      </div>
    </common-dialog>
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script setup>
import crudApi, { editStatus, saveJobUser, getJobUser } from '@/api/user-manage/job'
import { getDeptAllUser } from '@/api/common'
import { ref, nextTick } from 'vue'

import { jobConfigPM as permission } from '@/page-permission/user'
import { systemEnabledEnum } from '@enum-ms/system'
import checkPermission from '@/utils/system/check-permission'

import { ElMessageBox, ElMessage } from 'element-plus'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

const tableRef = ref()
const userVisible = ref(false)
const userList = ref([])
const checkedList = ref([])
const tree = ref()
const submitLoading = ref(false)
const currentRow = ref({})
const bindUserList = ref({})
const bindUserVisible = ref(false)
const defaultProps = {
  children: 'children',
  label: 'userName'
}
const { crud, columns, CRUD } = useCRUD(
  {
    title: '岗位',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  paginate: true
})

async function changeStatus(data, val) {
  try {
    await ElMessageBox.confirm('此操作将 "' + systemEnabledEnum.VL[val] + '" ' + data.name + ', 是否继续？', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    const submitData = {
      id: data.id,
      enabled: data.enabled
    }
    await editStatus(submitData)
    crud.refresh()
    crud.notify(systemEnabledEnum.VL[val] + '成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('变更岗位状态', error, val)
    data.enabled = data.enabled === systemEnabledEnum.TRUE.V ? systemEnabledEnum.FALSE.V : systemEnabledEnum.TRUE.V
  }
}

async function getDeptUser(row) {
  currentRow.value = row
  userList.value = []
  try {
    const data = await getDeptAllUser({ deptId: row.deptId, jobId: row.id })
    const content = {}
    content.userName = '<部门>' + data?.deptName
    content.userId = '01_' + data?.id
    content.children = []
    content.children.push({
      userName: '<岗位>' + data?.jobName,
      userId: '02_' + data?.jobId,
      children: data.userList
    })
    userList.value.push(content)
    if (data.userList?.length > 0) {
      data.userList.map(v => {
        if (v.jobId === data.jobId) {
          checkedList.value.push(v.userId)
        }
      })
    }
    userVisible.value = true
    nextTick(() => {
      tree.value.setCheckedKeys(checkedList.value)
    })
  } catch (error) {
    console.log('用户部门信息', error)
  }
}

function handleClose() {
  checkedList.value = []
  tree.value.setCheckedKeys(checkedList.value)
  userVisible.value = false
}

function closeBind() {
  bindUserVisible.value = false
}

async function userSubmit() {
  try {
    submitLoading.value = true
    let checkedNodes = tree.value.getCheckedKeys(true)
    checkedNodes = checkedNodes.filter((v) => v > 0)
    await saveJobUser({ jobId: currentRow.value.id, userIds: checkedNodes })
    checkedList.value = checkedNodes
    ElMessage({
      message: '人员配置成功',
      type: 'success'
    })
    userVisible.value = false
  } catch (error) {
    console.log('人员配置', error)
  } finally {
    submitLoading.value = false
  }
}

async function getJobBindUser(row) {
  bindUserList.value = {}
  try {
    const data = await getJobUser({ jobId: row.id }) || {}
    bindUserList.value = data
    bindUserList.value.deptName = data.deptName || row.deptName
    bindUserList.value.jobName = data.jobName || row.name
    bindUserVisible.value = true
  } catch (error) {
    console.log('岗位人员设置', error)
  }
}
</script>
