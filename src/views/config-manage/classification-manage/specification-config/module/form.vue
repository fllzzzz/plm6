<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.status.cu > 0"
    :before-close="crud.cancelCU"
    :title="crud.status.title"
    :show-close="true"
    :size="1000"
  >
    <template #titleRight>
      <common-button size="mini" type="success" icon="el-icon-plus" @click="addRow(form.list)" />
      <common-button :loading="crud.status.cu === 2" size="mini" type="primary" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <div class="spec-main-content">
        <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="80px">
          <el-form-item label="规格名称" prop="name">
            <el-input
              v-model="form.name"
              type="text"
              placeholder="请填写规格名称"
              size="small"
              maxlength="10"
              class="input-underline"
              style="width: 350px"
            />
          </el-form-item>
          <!-- <el-form-item label="自定义" prop="customizeable">
            <el-radio-group v-model="form.customizeable">
              <el-radio-button :label="1">是</el-radio-button>
              <el-radio-button :label="0">否</el-radio-button>
            </el-radio-group>
          </el-form-item> -->
          <el-form-item label="加权平均" prop="boolWeightedAverage">
            <common-radio-button
              v-model="form.boolWeightedAverage"
              :disabled="!checkPermission(permission.weightedAverage)"
              :options="boolWeightedAverageEnum.ENUM"
              type="enum"
            />
          </el-form-item>
          <common-table
            border
            :data="form.list"
            :cell-class-name="data => wrongCellMask(data, tableRules)"
            :max-height="maxHeight"
            row-key="uid"
          >
            <el-table-column label="序号" type="index" align="center" width="60" />
            <el-table-column prop="code" label="编码" align="center" width="100">
              <template v-slot="scope">
                <el-input
                  v-model.trim="scope.row.code"
                  :readonly="scope.row.boolUsed"
                  type="text"
                  placeholder="编码"
                  size="mini"
                  maxlength="3"
                  class="input-underline"
                  style="width: 100%;"
                />
              </template>
            </el-table-column>
            <el-table-column prop="value" label="规格" align="center" min-width="180">
              <template v-slot="scope">
                <el-input
                  v-model.trim="scope.row.value"
                  :readonly="scope.row.boolUsed"
                  type="text"
                  placeholder="规格"
                  size="mini"
                  maxlength="20"
                  class="input-underline"
                  style="width: 100%"
                />
              </template>
            </el-table-column>
            <el-table-column label="操作" align="center" width="80">
              <template v-slot="scope">
                <common-button
                v-if="!scope.row.boolUsed"
                  icon="el-icon-minus"
                  type="danger"
                  style="padding: 5px"
                  size="mini"
                  @click="removeRow(form.list, scope.$index)"
                />
                <svg-icon v-else class="icon icon-readonly" icon-class="readonly" />
              </template>
            </el-table-column>
          </common-table>
        </el-form>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { inject, ref } from 'vue'
import { boolWeightedAverageEnum } from '@enum-ms/finance'
import checkPermission from '@/utils/system/check-permission'

import { regForm } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useTableOperate from '@compos/form/use-table-operate'
import useTableValidate from '@compos/form/use-table-validate'
import SvgIcon from '@comp/SvgIcon/index.vue'

const currentNode = inject('currentNode')
const permission = inject('permission')

const defaultForm = {
  name: '', // 规格名称
  classificationId: undefined, // 科目id
  boolWeightedAverage: boolWeightedAverageEnum.TRUE.V, // 是否参加加权平均
  list: [] // 具体规格列表
}

// from.list => 行数据
const defaultRow = {
  code: undefined,
  value: undefined,
  sort: undefined
}

const rules = {
  name: [{ required: true, message: '请输入规格名称', trigger: 'blur' }],
  boolWeightedAverage: [{ required: true, message: '请选择是否参加加权平均', trigger: 'change' }]
}

const tableRules = {
  code: [{ required: true, message: '请输入编码', trigger: 'blur' }],
  value: [{ required: true, message: '请输入规格', trigger: 'blur' }]
}

const drawerRef = ref()
const formRef = ref()

const { maxHeight } = useMaxHeight({
  extraBox: '.el-drawer__header',
  wrapperBox: ['.el-drawer__body', '.spec-main-content'],
  navbar: false,
  extraHeight: 120,
  minHeight: 300
}, () => drawerRef.value.loaded)

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const { init, addRow, removeRow } = useTableOperate(defaultRow, 10)

// 初始化表单
CRUD.HOOK.afterToAdd = () => {
  init(form.list)
}
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules })

CRUD.HOOK.beforeValidateCU = () => {
  const { validResult, dealList } = tableValidate(form.list)
  if (validResult) {
    form.list = dealList
  } else {
    return validResult
  }
}

// 表单提交数据清理
crud.submitFormFormat = (form) => {
  form.classificationId = currentNode.value.id
  cleanUpData(form.list)
  return form
}
</script>

<style lang="scss" scoped>
.spec-main-content {
  padding: 10px 20px 00px 20px;
  ::v-deep(.el-input-number .el-input__inner) {
    text-align: left;
  }
  ::v-deep(.el-button--mini) {
    min-height:20px;
  }
}
</style>
