<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="800px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <span style="font-size: 12px; color: red">*配套件的编号不得与构件、部件、零件重名</span>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px" style="margin">
        <el-form-item label="配套件科目匹配" prop="classifyIds">
          <common-radio-button
            v-model="form.basicClass"
            :options="[matClsEnum.MATERIAL]"
            type="enum"
            size="small"
            style="margin-bottom: 5px"
          />
          <br />
          <material-cascader
            v-model="form.classifyIds"
            :basic-class="form.basicClass"
            :disabled="!form.basicClass"
            multiple
            :collapse-tags="false"
            separator=" > "
            clearable
            :disabledVal="disabledClassifyIds"
            placeholder="请选择科目"
            size="small"
            style="width: 270px"
          />
        </el-form-item>
        <el-form-item label="类型命名" prop="name">
          <el-input v-model.trim="form.name" type="text" placeholder="类型命名" style="width: 270px" maxlength="30" />
        </el-form-item>
        <el-form-item label="所属类型" prop="type">
          <common-select
            v-model="form.type"
            :options="auxiliaryMaterialTypeEnum.ENUM"
            type="enum"
            size="small"
            class="filter-item"
            placeholder="所属类型选择"
            style="width: 270px"
          />
        </el-form-item>
        <el-form-item label="排序" prop="sort">
          <el-input-number v-model.number="form.sort" :min="1" :max="999" :step="1" controls-position="right" style="width: 270px" />
        </el-form-item>
        <el-form-item label="编号" prop="list">
          <div class="process-container">
            <div class="process-box">
              <div v-for="(item, index) in form.list" :key="index" class="process-drawer">
                <el-input
                  v-model="item.serialNumber"
                  type="text"
                  placeholder="编号"
                  style="width: 270px; margin-right: 5px"
                  @blur="checkName(item, index)"
                />
                <common-button
                  v-show="form.list && form.list.length > 1"
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
import { ref, nextTick, defineProps, watch } from 'vue'
import { ElMessage } from 'element-plus'
import { deepClone } from '@data-type/index'
import { matClsEnum } from '@enum-ms/classification'
import { auxiliaryMaterialTypeEnum } from '@enum-ms/mes'
import MaterialCascader from '@comp-cls/material-cascader/index.vue'
import { regForm } from '@compos/use-crud'

const props = defineProps({
  boundAllClassifyIds: {
    type: Array,
    default: () => []
  }
})
const disabledClassifyIds = ref([])
const formRef = ref()
const nameArr = ref([])
const defaultForm = {
  id: undefined,
  name: '',
  basicClass: matClsEnum.MATERIAL.V,
  type: undefined,
  sort: undefined,
  list: []
}
const { crud, form, CRUD } = regForm(defaultForm, formRef)

const validateLinks = (rule, value, callback) => {
  if (value && value.length) {
    for (const i in value) {
      if (!value[i].serialNumber) {
        callback(new Error('请填写编号'))
      } else {
        callback()
      }
    }
    callback()
  } else {
    callback(new Error('请填写编号'))
  }
}

const rules = {
  name: [
    { required: true, message: '请填写类型名称', trigger: 'blur' },
    { min: 1, max: 30, message: '长度在 1 到 30 个字符', trigger: 'blur' }
  ],
  classifyIds: [{ required: true, message: '请选择科目', trigger: 'change' }],
  type: [{ required: true, message: '请选择所属类型', trigger: 'change' }],
  list: [
    { required: true, message: '请填写编号' },
    { validator: validateLinks, trigger: 'change' }
  ]
}

watch(
  () => form.basicClass,
  (val, oldVal) => {
    if (oldVal === matClsEnum.STEEL_PLATE.V) {
      nameArr.value = []
      form.list = []
    }
    if (val === matClsEnum.STEEL_PLATE.V && oldVal && oldVal !== matClsEnum.STEEL_PLATE.V) {
      nameArr.value = []
      form.list = []
    }
    if (val && oldVal) {
      form.classifyIds = []
    }
  }
)

function addProcess() {
  form.list.push({
    serialNumber: undefined
  })
}
function delProcess(index) {
  form.list.splice(index, 1)
}

function checkName(item, index) {
  const val = nameArr.value.find((v) => v.index === index)
  if (val) {
    if (item.serialNumber) {
      if (val.serialNumber === item.serialNumber) {
        return
      }
      if (nameArr.value.findIndex((v) => v.serialNumber === item.serialNumber) > -1) {
        ElMessage({
          message: '编号已存在，请重新填写',
          type: 'error'
        })
        item.serialNumber = undefined
        val.serialNumber = undefined
      } else {
        val.serialNumber = item.serialNumber
      }
    } else {
      val.serialNumber = undefined
    }
  } else {
    if (item.serialNumber) {
      if (nameArr.value.findIndex((v) => v.serialNumber === item.serialNumber) > -1) {
        ElMessage({
          message: '编号已存在，请重新填写',
          type: 'error'
        })
        form.list[index].serialNumber = undefined
      }
      nameArr.value.push({
        serialNumber: item.serialNumber,
        index: index
      })
    }
  }
}

CRUD.HOOK.beforeSubmit = () => {
  form.auxiliarySerialNumberList = []
  form.list.map((v) => {
    form.auxiliarySerialNumberList.push(v.serialNumber)
  })
}

CRUD.HOOK.beforeToAdd = () => {
  nameArr.value = []
}

CRUD.HOOK.beforeToEdit = () => {
  nameArr.value = []
  form.list.map((v, index) => {
    nameArr.value.push({
      serialNumber: v.serialNumber,
      index: index
    })
  })
}
CRUD.HOOK.beforeToCU = () => {
  nextTick(() => {
    disabledClassifyIds.value = deepClone(props.boundAllClassifyIds)
    form.classifyIds &&
      form.classifyIds.forEach((v) => {
        const _index = disabledClassifyIds.value.indexOf(v)
        if (_index !== -1) {
          disabledClassifyIds.value.splice(_index, 1)
        }
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
.add-row-box {
  text-align: center;
}
</style>
