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
        <el-form-item label="构件种类" prop="classificationName">
          <el-input v-model="form.classificationName" type="text" placeholder="构件种类" style="width: 270px" maxlength="30" />
        </el-form-item>
        <el-form-item label="排序" prop="sort">
          <el-input-number v-model.number="form.sort" :min="1" :max="999" :step="1" controls-position="right" style="width: 270px" />
        </el-form-item>
        <el-form-item label="构件前缀字母" prop="specPrefixList">
          <div class="process-container">
            <div class="process-box">
              <div v-for="(item, index) in form.specPrefixList" :key="index" class="process-drawer">
                <el-input
                  v-model="item.specPrefix"
                  type="text"
                  placeholder="大写字母"
                  style="width: 270px; margin-right: 5px"
                  @blur="checkName(item, index)"
                />
                <common-select
                  v-model="item.boolUseAssemble"
                  :options="whetherEnum.ENUM"
                  type="enum"
                  size="small"
                  clearable
                  class="filter-item"
                  placeholder="是否匹配组立"
                  style="width: 250px"
                  @change="item.add = false"
                />
                <common-button
                  v-show="form.specPrefixList && form.specPrefixList.length > 1"
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
import { ElMessage } from 'element-plus'

import { isNotBlank } from '@data-type/index'
import { whetherEnum } from '@enum-ms/common'

import { regForm } from '@compos/use-crud'

const formRef = ref()
const nameArr = ref([])
const defaultForm = {
  id: undefined,
  classificationName: '',
  sort: 1,
  specPrefixList: []
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)
const validateLinks = (rule, value, callback) => {
  if (value && value.length) {
    for (const i in value) {
      if (!value[i].add) {
        if (!value[i].specPrefix) {
          callback(new Error('请填写大写关键字母'))
        }
        if (!isNotBlank(value[i].boolUseAssemble)) {
          callback(new Error('请选择是否匹配组立'))
        }
      } else {
        callback()
      }
    }
    callback()
  } else {
    callback(new Error('请填写大写前缀字母'))
  }
}

const rules = {
  classificationName: [
    { required: true, message: '请填写构件种类名称', trigger: 'blur' },
    { min: 1, max: 30, message: '长度在 1 到 30 个字符', trigger: 'blur' }
  ],
  sort: [{ required: true, message: '请填写排序值', trigger: 'blur', type: 'number' }],
  specPrefixList: [
    { required: true, message: '请填写前缀字母' },
    { validator: validateLinks, trigger: 'change' }
  ]
}

function addProcess() {
  form.specPrefixList.push({
    add: true
  })
}
function delProcess(index) {
  form.specPrefixList.splice(index, 1)
}

function checkName(item, index) {
  item.add = false
  const val = nameArr.value.find((v) => v.index === index)
  if (val) {
    if (item.specPrefix) {
      if (val.specPrefix === item.specPrefix) {
        return
      }
      if (nameArr.value.findIndex((v) => v.specPrefix === item.specPrefix) > -1) {
        ElMessage({
          message: '前缀字母已存在，请重新填写',
          type: 'error'
        })
        item.specPrefix = undefined
        val.specPrefix = undefined
      } else {
        if (!/^[A-Z]+$/.test(item.specPrefix)) {
          form.specPrefixList[index].specPrefix = undefined
          val.specPrefix = undefined
          return
        }
        val.specPrefix = item.specPrefix
      }
    } else {
      val.specPrefix = undefined
    }
  } else {
    if (item.specPrefix) {
      if (!/^[A-Z]+$/.test(item.specPrefix)) {
        form.specPrefixList[index].specPrefix = undefined
        return
      }
      if (nameArr.value.findIndex((v) => v.specPrefix === item.specPrefix) > -1) {
        ElMessage({
          message: '前缀字母已存在，请重新填写',
          type: 'error'
        })
        form.specPrefixList[index].specPrefix = undefined
      }
      nameArr.value.push({
        specPrefix: item.specPrefix,
        index: index
      })
    }
  }
}

CRUD.HOOK.beforeValidateCU = (crud, form) => {
  if (crud.form.specPrefixList && crud.form.specPrefixList.length > 0) {
    crud.form.specPrefixList.map((v) => {
      v.add = false
    })
  }
}

CRUD.HOOK.afterToAdd = () => {
  nameArr.value = []
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
