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
        <el-form-item label="钢材分类名称" prop="name">
          <el-input v-model="form.name" type="text" placeholder="钢材分类名称" style="width: 270px" maxlength="30" />
        </el-form-item>
        <el-form-item label="钢材科目" prop="classifyIds">
          <material-cascader
            v-model="form.classifyIds"
            :basic-class="(matClsEnum.STEEL_PLATE.V | matClsEnum.SECTION_STEEL.V |matClsEnum.STEEL_COIL.V)"
            multiple
            :collapse-tags="false"
            separator=" > "
            clearable
            :disabledVal="disabledClassifyIds"
            placeholder="请选择钢材科目"
            size="small"
            style="width: 270px"
          />
        </el-form-item>
        <el-form-item label="排序" prop="sort">
          <el-input-number v-model.number="form.sort" :min="1" :max="999" :step="1" controls-position="right" style="width: 270px" />
        </el-form-item>
        <el-form-item label="关键字母" prop="links">
          <div class="process-container">
            <div class="process-box">
              <div v-for="(item, index) in form.links" :key="index" class="process-drawer">
                <el-input
                  v-model="item.keyword"
                  type="text"
                  placeholder="大写字母"
                  style="width: 270px;margin-right:5px;"
                  @blur="checkName(item, index)"
                />
                <common-select
                  v-model="item.specIndex"
                  :options="specIndexEnum"
                  showOptionAll
                  :allVal="0"
                  type="enum"
                  size="small"
                  clearable
                  class="filter-item"
                  placeholder="索引"
                  style="width: 250px"
                  @change="item.add = false"
                />
                <common-button
                  v-show="form.links && form.links.length > 1"
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
import { ref, defineProps, nextTick } from 'vue'
import { ElMessage } from 'element-plus'

import { isNotBlank, deepClone } from '@data-type/index'
import { matClsEnum } from '@enum-ms/classification'

import { regForm } from '@compos/use-crud'
import MaterialCascader from '@comp-cls/material-cascader/index.vue'

const props = defineProps({
  boundAllClassifyIds: {
    type: Array,
    default: () => {}
  }
})

const formRef = ref()
const nameArr = ref([])
const disabledClassifyIds = ref([])
const specIndexEnum = {
  1: { L: '1', K: '1', V: 1 },
  2: { L: '2', K: '2', V: 2 },
  3: { L: '3', K: '3', V: 3 },
  4: { L: '4', K: '4', V: 4 }
}
const defaultForm = {
  id: undefined,
  name: '',
  sort: 1,
  links: []
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)
const validateLinks = (rule, value, callback) => {
  if (value && value.length) {
    for (const i in value) {
      if (!value[i].add) {
        if (!value[i].keyword) {
          callback(new Error('请填写大写关键字母'))
        }
        if (!isNotBlank(value[i].specIndex)) {
          callback(new Error('请选择索引'))
        }
      } else {
        callback()
      }
    }
    callback()
  } else {
    callback(new Error('请填写大写关键字母'))
  }
}

const rules = {
  name: [
    { required: true, message: '请填写钢材分类名称', trigger: 'blur' },
    { min: 1, max: 30, message: '长度在 1 到 30 个字符', trigger: 'blur' }
  ],
  sort: [{ required: true, message: '请填写排序值', trigger: 'blur', type: 'number' }],
  classifyIds: [{ required: true, message: '请选择绑定的钢材科目', trigger: 'change' }],
  links: [
    { required: true, message: '请选择关键字母' },
    { validator: validateLinks, trigger: 'change' }
  ]
}

function addProcess() {
  form.links.push({
    add: true
  })
}
function delProcess(index) {
  form.links.splice(index, 1)
}

function checkName(item, index) {
  item.add = false
  const val = nameArr.value.find((v) => v.index === index)
  if (val) {
    if (item.keyword) {
      if (val.keyword === item.keyword) {
        return
      }
      if (nameArr.value.findIndex((v) => v.keyword === item.keyword) > -1) {
        ElMessage({
          message: '关键字母已存在，请重新填写',
          type: 'error'
        })
        item.keyword = undefined
        val.keyword = undefined
      } else {
        if (!/^[A-Z]+$/.test(item.keyword)) {
          form.links[index].keyword = undefined
          val.keyword = undefined
          return
        }
        val.keyword = item.keyword
      }
    } else {
      val.keyword = undefined
    }
  } else {
    if (item.keyword) {
      if (!/^[A-Z]+$/.test(item.keyword)) {
        form.links[index].keyword = undefined
        return
      }
      if (nameArr.value.findIndex((v) => v.keyword === item.keyword) > -1) {
        ElMessage({
          message: '关键字母已存在，请重新填写',
          type: 'error'
        })
        form.links[index].keyword = undefined
      }
      nameArr.value.push({
        keyword: item.keyword,
        index: index
      })
    }
  }
}

CRUD.HOOK.beforeValidateCU = (crud, form) => {
  if (crud.form.links && crud.form.links.length > 0) {
    crud.form.links.map((v) => {
      v.add = false
    })
  }
}

CRUD.HOOK.afterToAdd = () => {
  nameArr.value = []
}

CRUD.HOOK.beforeToCU = () => {
  nextTick(() => {
    disabledClassifyIds.value = deepClone(props.boundAllClassifyIds)
    form.classifyIds && form.classifyIds.forEach((v) => {
      const _index = disabledClassifyIds.value.indexOf(v)
      if (_index !== -1) { disabledClassifyIds.value.splice(_index, 1) }
    })
  })
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
