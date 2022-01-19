<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="'新增'"
    width="500px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="60px">
      <el-form-item label="编码" prop="code">
        <el-input v-model="form.code" type="text" placeholder="请填写编码" style="width: 200px" />
      </el-form-item>
      <el-form-item label="排序" prop="sort">
        <el-input-number
          class="align-left"
          v-model="form.sort"
          placeholder="请填写"
          type="text"
          controls-position="right"
          style="width: 200px"
          :min="0"
          :max="10000"
        />
      </el-form-item>
      <common-table
          :data="form.dictionaryDetails"
          empty-text="暂无数据"
          :max-height="500"
          :cell-class-name="wrongCellMask"
          style="width: 100%"
        >
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column key="remark" prop="remark" :show-overflow-tooltip="true" label="名称">
            <template v-slot="scope">
              <span>{{scope.row.remark}}</span>
            </template>
          </el-table-column>
          <el-table-column :show-overflow-tooltip="true" prop="label" label="值" align="center">
            <template v-slot="scope">
              <el-input-number
                class="align-left"
                v-model="scope.row.label"
                placeholder="请填写"
                type="text"
                controls-position="right"
                style="width: 100%"
                :min="0"
              />
            </template>
          </el-table-column>
        </common-table>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'
import { regForm } from '@compos/use-crud'
import useTableValidate from '@compos/form/use-table-validate'

const formRef = ref()

const defaultForm = {
  code: undefined,
  sort: undefined,
  dictionaryDetails: [
    // { id: '', name: 'high', remark: '高度', label: undefined, sort: 1 },
    { id: '', name: 'effectiveWidth', remark: '有效宽度(mm)', label: undefined, sort: 1 },
    { id: '', name: 'firstQuarter', remark: '上弦筋(φ)', label: undefined, sort: 2 },
    { id: '', name: 'lastQuarter', remark: '下弦筋(φ)', label: undefined, sort: 3 },
    { id: '', name: 'webMember', remark: '腹杆筋(φ)', label: undefined, sort: 4 },
    { id: '', name: 'vertical', remark: '竖向筋(φ)', label: undefined, sort: 5 },
    { id: '', name: 'level', remark: '水平筋(φ)', label: undefined, sort: 6 },
    { id: '', name: 'basementMembrane', remark: '底膜(mm)', label: undefined, sort: 7 },
    { id: '', name: 'weightMeter', remark: '米重(kg/m)', label: undefined, sort: 8 }
  ]
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const tableRules = {
  label: [{ required: true, max: 20, message: '不能超过50个字符', trigger: 'blur', type: 'number' }]
}
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })

const rules = {
  code: [
    { required: true, message: '请填写编码', trigger: 'blur' },
    { min: 1, max: 32, message: '长度在 1 到 32 个字符', trigger: 'blur' }
  ],
  sort: [{ required: true, message: '请输入排序', trigger: 'blur', type: 'number' }]
}

CRUD.HOOK.beforeSubmit = () => {
  const { validResult, dealList } = tableValidate(form.dictionaryDetails)
  if (validResult) {
    form.dictionaryDetails = dealList
  } else {
    return validResult
  }
}

</script>

<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
