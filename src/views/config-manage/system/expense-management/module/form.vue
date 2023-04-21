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
        <el-form-item label="费用归属" prop="costAscriptionEnum">
          <common-select
            v-model="form.costAscriptionEnum"
            :options="costAscriptionEnum.ENUM"
            type="enum"
            size="small"
            clearable
            placeholder="请选择费用归属"
            style="width: 270px"
          />
        </el-form-item>
        <el-form-item label="费用类型" prop="name">
          <el-input v-model="form.name" type="text" placeholder="请填写费用类型" style="width: 270px" maxlength="30" />
        </el-form-item>
        <el-form-item label="排序" prop="sort">
          <el-input-number v-model.number="form.sort" :min="1" :max="999" :step="1" controls-position="right" style="width: 270px" />
        </el-form-item>
        <el-form-item label="费用明细" prop="dictionaryIdList">
          <div class="process-container">
            <div class="process-box">
              <div v-for="(item, index) in form.dictionaryIdList" :key="index" class="process-drawer">
                <common-select
                  v-model="form.dictionaryIdList[index]"
                  :options="dict.reimbursement_type"
                  type="other"
                  size="small"
                  :dataStructure="typeProp"
                  clearable
                  class="filter-item"
                  placeholder="费用类型"
                  style="width: 250px"
                  :disabled-val="form.dictionaryIdList"
                />
                <common-button
                  v-show="form.dictionaryIdList && form.dictionaryIdList.length > 1"
                  icon="el-icon-delete"
                  size="mini"
                  type="danger"
                  style="margin-left: 6px"
                  @click="delProcess(index)"
                />
              </div>
            </div>
            <common-button icon="el-icon-plus" size="mini" type="success" style="margin: 0 0 12px 6px" @click="addProcess" />
          </div>
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref } from 'vue'

import { costAscriptionEnum } from '@enum-ms/config'

import { regForm } from '@compos/use-crud'
import useDict from '@compos/store/use-dict'

const formRef = ref()
const dict = useDict(['reimbursement_type'])
const typeProp = { key: 'id', label: 'label', value: 'id' }
const defaultForm = {
  iid: undefined,
  name: '',
  sort: 1,
  costAscriptionEnum: undefined,
  dictionaryIdList: []
}

const { crud, form } = regForm(defaultForm, formRef)
const validateLinks = (rule, value, callback) => {
  if (!value?.length) {
    callback(new Error('请选择费用类型'))
  }
  if (value && value.length) {
    for (const i in value) {
      if (!value[i]) {
        callback(new Error('请选择费用类型'))
      }
    }
  }
  callback()
}
const rules = {
  costAscriptionEnum: [{ required: true, message: '请选择费用归属', trigger: 'change' }],
  name: [
    { required: true, message: '请填写费用类型', trigger: 'blur' },
    { min: 1, max: 30, message: '长度在 1 到 30 个字符', trigger: 'blur' }
  ],
  sort: [{ required: true, message: '请填写排序值', trigger: 'blur', type: 'number' }],
  dictionaryIdList: [{ validator: validateLinks, trigger: 'change' }]
}

function addProcess() {
  form.dictionaryIdList.push(undefined)
}
function delProcess(index) {
  form.dictionaryIdList.splice(index, 1)
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
.process-container {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-end;
  .process-box {
    display: flex;
    flex-direction: column;
    justify-content: flex-start;
    align-items: flex-start;
    .process-drawer {
      display: flex;
      flex-direction: row;
      justify-content: flex-start;
      align-items: center;
      margin-bottom: 10px;
    }
  }
}
</style>
