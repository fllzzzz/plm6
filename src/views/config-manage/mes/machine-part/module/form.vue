<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="650px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
        <el-form-item label="代表部件类型" prop="name">
          <el-input v-model="form.name" type="text" placeholder="代表部件类型" style="width: 270px" maxlength="30" />
        </el-form-item>
        <el-form-item label="排序" prop="sort">
          <el-input-number v-model.number="form.sort" :min="1" :max="999" :step="1" controls-position="right" style="width: 270px" />
        </el-form-item>
        <common-table
          ref="detailRef"
          border
          :data="form.assembleSpecList"
          :max-height="maxHeight"
          style="width: 100%;margin-top:10px;"
          class="table-form"
          return-source-data
          :showEmptySymbol="false"
          :cell-class-name="wrongCellMask"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column key="specPrefix" prop="specPrefix" label="*部件规格前缀(大写)" align="center">
            <template v-slot="scope">
              <el-input v-model.trim="scope.row.specPrefix" type="text" placeholder="请填写大写字母" maxlength="10" @blur="checkName(scope.row,scope.$index)"/>
            </template>
          </el-table-column>
          <!-- <el-table-column key="boolSchedulingEnum" prop="boolSchedulingEnum" label="*是否有生成工序" align="center">
            <template v-slot="scope">
              <common-radio v-model="scope.row.boolSchedulingEnum" :options="whetherEnum.ENUM" type="enum" />
            </template>
          </el-table-column> -->
          <el-table-column label="操作" align="center" width="100">
            <template v-slot="scope">
              <common-button size="small" class="el-icon-delete" type="danger" @click="deleteRow(scope.$index)" />
            </template>
          </el-table-column>
        </common-table>
        <div class="add-row-box">
          <common-button
            size="mini"
            icon="el-icon-circle-plus-outline"
            type="warning"
            style="margin-top: 15px"
            @click="addRow()"
            >添加</common-button>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref } from 'vue'
import { ElMessage } from 'element-plus'

// import { whetherEnum } from '@enum-ms/common'
import useMaxHeight from '@compos/use-max-height'
import useTableValidate from '@compos/form/use-table-validate'

import { regForm } from '@compos/use-crud'

const formRef = ref()
const nameArr = ref([])
const defaultForm = {
  id: undefined,
  name: '',
  sort: undefined,
  assembleSpecList: []
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

const rules = {
  name: [
    { required: true, message: '请填写代表部件类型名称', trigger: 'blur' },
    { min: 1, max: 30, message: '长度在 1 到 30 个字符', trigger: 'blur' }
  ],
  sort: [{ required: true, message: '请填写排序值', trigger: 'blur', type: 'number' }]
}

const { maxHeight } = useMaxHeight({
  wrapperBox: '.addForm',
  paginate: true,
  extraHeight: 120
})

const tableRules = {
  specPrefix: [{ required: true, message: '请输入部件号规格前缀', trigger: 'blur' }]
  // boolSchedulingEnum: [{ required: true, message: '请选择是否有生成工序', trigger: 'change' }]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验

function addRow() {
  form.assembleSpecList.push({
    add: true
  })
}
function deleteRow(index) {
  form.assembleSpecList.splice(index, 1)
}

function checkName(item, index) {
  const val = nameArr.value.find((v) => v.index === index)
  if (val) {
    if (item.specPrefix) {
      if (val.specPrefix === item.specPrefix) {
        return
      }
      if (nameArr.value.findIndex((v) => v.specPrefix === item.specPrefix) > -1) {
        ElMessage({
          message: '规格前缀已存在，请重新填写',
          type: 'error'
        })
        item.specPrefix = undefined
        val.specPrefix = undefined
      } else {
        if (!/^[A-Z]+$/.test(item.specPrefix)) {
          form.assembleSpecList[index].specPrefix = undefined
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
        form.assembleSpecList[index].specPrefix = undefined
        return
      }
      if (nameArr.value.findIndex((v) => v.specPrefix === item.specPrefix) > -1) {
        ElMessage({
          message: '规格前缀已存在，请重新填写',
          type: 'error'
        })
        form.assembleSpecList[index].specPrefix = undefined
      }
      nameArr.value.push({
        specPrefix: item.specPrefix,
        index: index
      })
    }
  }
}

CRUD.HOOK.beforeValidateCU = (crud, form) => {
  if (crud.form.assembleSpecList && crud.form.assembleSpecList.length === 0) {
    ElMessage.error('请填写部件号规格前缀明细')
    return false
  }
  const { validResult, dealList } = tableValidate(crud.form.assembleSpecList)
  if (validResult) {
    crud.form.assembleSpecList = dealList
  } else {
    return validResult
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
.add-row-box{
  text-align: center;
}
</style>
