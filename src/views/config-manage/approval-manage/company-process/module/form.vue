<template>
  <common-drawer
    append-to-body
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    size="40%"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="80px">
        <div class="table-header">
          <el-form-item label="审批类型" prop="type">
            <common-select
              v-model="form.type"
              :options="approvalTypeOptions"
              type="other"
              :dataStructure="{ key: 'id', label: 'showName', value: 'id' }"
              size="small"
              placeholder="选择审批类型"
              @change="approvalTypeChange"
              style="width:320px"
            />
          </el-form-item>
          <el-form-item label="审批名称" prop="approveInfoName">
            <el-input v-model.trim="form.approveInfoName" type="text" maxlength="30" size="small" placeholder="审批名称" style="width: 320px" />
          </el-form-item>
          <el-form-item label="发起人" prop="applicantIds">
            <user-dept-cascader
              v-model="form.applicantIds"
              :filterNotDdUser="true"
              multiple
              filterable
              clearable
              show-all-levels
              placeholder="选择发起人"
              style="width: 320px"
            />
          </el-form-item>
          <!-- <el-form-item label="抄送节点" prop="ccPosition">
            <common-select
              v-model="form.ccPosition"
              :options="ddApprovalPositionEnum.ENUM"
              clearable
              type="enum"
              size="small"
              class="filter-item"
              placeholder="选择抄送节点"
              style="width: 320px"
            />
          </el-form-item>
          <el-form-item label="抄送人" prop="ccUserIds">
            <user-dept-cascader
              v-model="form.ccUserIds"
              :filterNotDdUser="true"
              multiple
              filterable
              clearable
              show-all-levels
              placeholder="选择抄送人"
              style="width: 320px"
            />
          </el-form-item> -->
          <div style="padding-bottom: 10px;">
            <el-divider><span class="title">审批流程</span></el-divider>
          </div>
        </div>
        <common-table
          ref="detailRef"
          border
          :data="approveList"
          :max-height="maxHeight"
          style="width: 100%"
          class="table-form"
          return-source-data
          :showEmptySymbol="false"
          :cell-class-name="wrongCellMask"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column key="type" prop="type" label="审批类型" align="center" min-width="100">
            <template v-slot="scope">
              <common-select
                v-model="scope.row.type"
                :options="ddTaskActionTypeEnum.ENUM"
                type="enum"
                size="small"
                placeholder="选择抄送节点"
                style="width:100%"
              />
            </template>
          </el-table-column>
          <el-table-column key="approval" prop="approval" label="审批人" align="left" min-width="160">
            <template v-slot="scope">
              <!-- 单签时：userId为number类型 -->
              <user-dept-cascader
                v-if="scope.row.type === ddTaskActionTypeEnum.NONE.V"
                v-model="scope.row.userId"
                :disabled="!scope.row.type"
                :filterNotDdUser="true"
                size="small"
                clearable
                show-all-levels
                style="width:100%"
                :placeholder="`${scope.row.type ? '选择一个审批人' : '先选择审批类型'}`"
              />
              <!-- 单签外：userIds为array类型 -->
              <user-dept-cascader
                v-else
                v-model="scope.row.userIds"
                :disabled="!scope.row.type"
                :filterNotDdUser="true"
                multiple
                size="small"
                clearable
                show-all-levels
                style="width:100%"
                :placeholder="`${scope.row.type ? '至少选择两个审批人' : '先选择审批类型'}`"
              />
            </template>
          </el-table-column>
          <el-table-column label="操作" width="80" align="center">
            <template v-slot="scope">
              <common-button size="small" class="el-icon-delete" type="danger" @click="deleteRow(scope.$index)" />
            </template>
          </el-table-column>
        </common-table>
        <div class="add-row-box">
          <common-button size="mini" icon="el-icon-circle-plus-outline" type="warning" style="margin-right:15px" @click="addRow()">继续添加</common-button>
          <el-tooltip
            effect="light"
            :content="`审批流程添加规则：\n
          1、审批类型为单签时，只能设置一个审批人；\n
          2、审批类型为会签、或签时，最少要设置两个审批人。\n`"
            placement="top"
          >
            <div style="display:inline-block;">
              <el-tag size="medium" type="info">审批流程添加规则</el-tag>
            </div>
          </el-tooltip>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref } from 'vue'
import { getApproveType } from '@/api/config/approval-config/company-process'

import { ddApproveTypeEnum, ddTaskActionTypeEnum } from '@enum-ms/dd'

import { regForm } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import { ElMessage } from 'element-plus'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import useTableValidate from '@compos/form/use-table-validate'

const formRef = ref()
const detailRef = ref()
const approvalTypeOptions = ref([])
const defaultTable = {
  type: void 0,
  userIds: [],
  userId: void 0
}
const approveList = ref([{
  ...defaultTable
}])

const defaultForm = {
  id: void 0,
  type: undefined,
  approvalCode: undefined,
  approveInfoName: undefined,
  // ccPosition: void 0, // 抄送节点
  // ccUserIds: [], // 抄送人
  applicantIds: [], // 发起人
  list: [] // 审批流程
}

// 审批人规则
const validateUserId = (value, row) => {
  if (row?.type) {
    if (row?.type === ddTaskActionTypeEnum.NONE.V) {
      return !!row.userId
    } else {
      // 非单签时，最少要选择2个人
      return (row.userIds instanceof Array) && row?.userIds?.length > 1
    }
  }
  return false
}

// 表格验证规则
const rules = {
  applicantIds: [
    { required: true, message: '发起人', trigger: 'blur', type: 'array' }
  ],
  type: [
    { required: true, message: '审批类型', trigger: 'change' }
  ],
  approveInfoName: [
    { required: true, message: '审批名称', trigger: 'blur' }
  ]
}

// 表格验证规则
const tableRules = {
  type: [
    { required: true, message: '请选择审批类型', trigger: 'blur', type: 'number' }
  ],
  approval: [
    { validator: validateUserId, message: '请选择审批人', trigger: 'blur' }
  ]
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    clientHRepMainH: true,
    navbar: false,
    minHeight: 300,
    extraHeight: 150
  }
)

// 编辑
CRUD.HOOK.afterToEdit = (crud, form) => {
  approveList.value = (form.approvers || []).map(row => {
    row.userId = undefined
    row.userIds = []
    if (row.type === ddTaskActionTypeEnum.NONE.V) {
      row.userId = row?.users?.[0]?.userId
    } else {
      row.userIds = row?.users?.map(v => v.userId)
    }
    return row
  })
}

// 提交
CRUD.HOOK.beforeSubmit = async () => {
  const list = approveList.value
  const { validResult } = tableValidate(list)
  if (!validResult) {
    return false
  }
  // 需要过滤没值的行
  crud.form.list = list.filter(v => v.type).map(v => {
    return {
      type: v.type,
      approverIds: v.type === ddTaskActionTypeEnum.NONE.V ? [v.userId] : v.userIds
    }
  })
}

// 删除
function deleteRow(index) {
  approveList.value.splice(index, 1)
}

// 新增
function addRow() {
  if (approveList.value.length > 19) {
    ElMessage({ message: '最多只能添加20条审批流程', type: 'warning' })
    return
  }
  approveList.value.push({
    ...defaultTable
  })
}

fetchApproveType()

async function fetchApproveType() {
  try {
    const data = await getApproveType() || []
    data.map(v => {
      v.showName = ddApproveTypeEnum.VL[v.id]
    })
    approvalTypeOptions.value = data
  } catch (error) {
    console.log('error', error)
  }
}

function approvalTypeChange(val) {
  const filterVal = approvalTypeOptions.value.find(v => v.id === val)
  form.approvalCode = filterVal?.name
}

</script>

<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
