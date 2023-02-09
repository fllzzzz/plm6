<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    ref="drawerRef"
    size="90%"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
        <common-table
          ref="detailRef"
          border
          :data="form.list"
          :max-height="maxHeight"
          style="width: 100%; margin-top: 10px"
          class="upload-table"
          return-source-data
          :showEmptySymbol="false"
          :cell-class-name="wrongCellMask"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column key="name" prop="name" label="名称" align="center">
            <template v-slot="scope">
              <el-input
                v-model.trim="scope.row.name"
                type="text"
                placeholder="请填写"
                maxlength="10"
              />
            </template>
          </el-table-column>
          <el-table-column key="serialNumber" prop="serialNumber" label="编号" align="center">
            <template v-slot="scope">
              <el-input
                v-model.trim="scope.row.serialNumber"
                type="text"
                placeholder="请填写"
                maxlength="10"
                :disabled="form.id && form.boolHaveHole"
              />
            </template>
          </el-table-column>
          <el-table-column key="specification" prop="specification" label="规格" align="center">
            <template v-slot="scope">
              <el-input
                v-model.trim="scope.row.specification"
                type="text"
                placeholder="请填写"
                maxlength="10"
              />
            </template>
          </el-table-column>
          <el-table-column key="length" prop="length" :label="'长度\n(mm)'" align="center">
            <template v-slot="scope">
              <el-input-number
                v-model.number="scope.row.length"
                :min="0"
                :max="9999999999"
                :step="1"
                :precision="DP.MES_ARTIFACT_L__MM"
                placeholder="请填写"
                controls-position="right"
              />
            </template>
          </el-table-column>
          <el-table-column key="thick" prop="thick" :label="'厚度\n(mm)'" align="center">
            <template v-slot="scope">
              <el-input-number
                v-model.number="scope.row.thick"
                :min="0"
                :max="9999999999"
                :step="1"
                :precision="DP.MES_ARTIFACT_T__MM"
                placeholder="请填写"
                controls-position="right"
              />
            </template>
          </el-table-column>
          <el-table-column key="material" prop="material" label="材质" align="center">
            <template v-slot="scope">
              <el-input
                v-model.trim="scope.row.material"
                type="text"
                placeholder="请填写"
                maxlength="10"
              />
            </template>
          </el-table-column>
          <el-table-column key="quantity" prop="quantity" label="数量" align="center">
            <template v-slot="scope">
              <el-input-number
                v-model.number="scope.row.quantity"
                :min="0"
                :max="9999999999"
                :step="1"
                :precision="0"
                placeholder="请填写"
                controls-position="right"
                @blur="weightChange(scope.row)"
              />
            </template>
          </el-table-column>
          <el-table-column key="netWeight" prop="netWeight" :label="`单净重\n(kg)`" align="left">
            <template v-slot="scope">
              <el-input-number
                v-model.number="scope.row.netWeight"
                :min="0"
                :max="9999999999"
                :step="1"
                :precision="DP.COM_WT__KG"
                placeholder="请填写"
                controls-position="right"
                size="mini"
                @blur="weightChange(scope.row)"
              />
            </template>
          </el-table-column>
          <el-table-column key="grossWeight" prop="grossWeight" :label="`单毛重\n(kg)`" align="left">
            <template v-slot="scope">
              <el-input-number
                v-model.number="scope.row.grossWeight"
                :min="0"
                :max="9999999999"
                :step="1"
                :precision="DP.COM_WT__KG"
                placeholder="请填写"
                controls-position="right"
                size="mini"
                @blur="weightChange(scope.row)"
              />
            </template>
          </el-table-column>
          <el-table-column
            key="totalNetWeight"
            prop="totalNetWeight"
            :label="`总净重\n(kg)`"
            align="left"
            min-width="95px"
          >
            <template v-slot="scope">
              {{ scope.row.totalNetWeight ? scope.row.totalNetWeight.toFixed(DP.COM_WT__KG) : '-' }}
            </template>
          </el-table-column>
          <el-table-column
            key="totalGrossWeight"
            prop="totalGrossWeight"
            :label="`总毛重\n(kg)`"
            align="left"
            min-width="95px"
          >
            <template v-slot="scope">
              {{ scope.row.totalGrossWeight ? scope.row.totalGrossWeight.toFixed(DP.COM_WT__KG) : '-' }}
            </template>
          </el-table-column>
          <el-table-column key="surfaceArea" prop="surfaceArea" :label="`面积\n(㎡)`" align="left" min-width="80px">
            <template v-slot="scope">
             <el-input-number
                v-model.number="scope.row.surfaceArea"
                :max="9999999999"
                :step="1"
                :precision="DP.COM_AREA__M2"
                placeholder="请填写"
                controls-position="right"
              />
            </template>
          </el-table-column>
          <el-table-column key="remark" prop="remark" label="备注" align="center">
            <template v-slot="scope">
              <el-input
                v-model.trim="scope.row.remark"
                type="text"
                placeholder="请填写"
                maxlength="100"
              />
            </template>
          </el-table-column>
          <el-table-column label="操作" align="center" width="100" v-if="!form.id">
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
            v-if="!form.id"
            >添加</common-button
          >
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref } from 'vue'
import { ElMessage } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import useTableValidate from '@compos/form/use-table-validate'
import { DP } from '@/settings/config'

import { regForm } from '@compos/use-crud'

const formRef = ref()
const drawerRef = ref()
const defaultForm = {
  id: undefined,
  list: []
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)
const rules = {}

const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body', '.upload-table'],
    navbar: false,
    extraHeight: 120
  },
  () => drawerRef.value.loaded
)

const tableRules = {
  name: [{ required: true, message: '请输入名称', trigger: 'blur' }],
  serialNumber: [{ required: true, message: '请输入编号', trigger: 'blur' }],
  specification: [{ required: true, message: '请输入规格', trigger: 'blur' }],
  material: [{ required: true, message: '请输入材质', trigger: 'blur' }],
  length: [{ required: true, message: '请输入长度', trigger: 'blur' }],
  thick: [{ required: true, message: '请输入厚度', trigger: 'blur' }],
  quantity: [{ required: true, message: '请输入数量', trigger: 'blur' }],
  grossWeight: [{ required: true, message: '请输入单毛重', trigger: 'blur' }],
  totalGrossWeight: [{ required: true, message: '请输入总毛重', trigger: 'blur' }],
  netWeight: [{ required: true, message: '请输入单净重', trigger: 'blur' }],
  totalNetWeight: [{ required: true, message: '请输入总净重', trigger: 'blur' }],
  surfaceArea: [{ required: true, message: '请输入面积', trigger: 'blur' }]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验

function addRow() {
  form.list.push({})
}
function deleteRow(index) {
  form.list.splice(index, 1)
}

function weightChange(row) {
  row.totalNetWeight = (row.quantity && row.netWeight) ? row.quantity * row.netWeight : undefined
  row.totalGrossWeight = (row.quantity && row.grossWeight) ? row.quantity * row.grossWeight : undefined
}

CRUD.HOOK.submit = (crud, form) => {
  if (crud.form.list && crud.form.list.length === 0) {
    ElMessage.error('请填写明细')
    return false
  }
  const { validResult, dealList } = tableValidate(crud.form.list)
  if (validResult) {
    crud.form.list = dealList
  } else {
    return validResult
  }
  if (crud.form.id) {
    crud.form = crud.form.list[0]
  }
}

CRUD.HOOK.afterToEdit = (crud, form) => {
  const value = JSON.parse(JSON.stringify(form))
  form.list = [value]
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input__inner) {
  text-align: left;
  padding-left:3px !important;
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
.upload-table {
  ::v-deep(.cell) {
    padding-left: 3px;
    padding-right: 3px;
  }
  ::v-deep(thead.is-group th) {
    background: #fff;
  }
}
</style>
