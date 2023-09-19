<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelBCU"
    :visible="dialogVisible"
    :title="crud.bStatus.title"
    :show-close="true"
    custom-class="supplier-batch-add"
    width="80%"
    top="10vh"
  >
    <template #titleRight>
      <common-button size="mini" type="success" icon="el-icon-plus" @click="addRow(form.list)" />
      <common-button :loading="crud.bStatus.cu === CRUD.STATUS.PROCESSING" type="primary" size="mini" @click="crud.submitBCU">
        提 交
      </common-button>
      <store-operation type="crudBatch" />
    </template>
    <div>
      <el-form ref="formRef" :model="form" :disabled="crud.bStatus.cu === CRUD.STATUS.PROCESSING">
        <common-table
          :data="form.list"
          return-source-data
          :show-empty-symbol="false"
          empty-text="暂无数据"
          :max-height="maxHeight"
          default-expand-all
          :cell-class-name="wrongCellMask"
          style="width: 100%"
          row-key="uid"
        >
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column prop="name" label="供应商名称" min-width="180">
            <template v-slot:header>
              <el-tooltip class="item" effect="light" content="长度在 2 到 32 个字符" placement="top">
                <div style="display: inline-block">
                  <span>供应商名称</span>
                  <i class="el-icon-info" />
                </div>
              </el-tooltip>
            </template>
            <template v-slot="scope">
              <el-input v-model="scope.row.name" size="small" style="width: 100%" placeholder="供应商名称" maxlength="32" clearable />
            </template>
          </el-table-column>
          <el-table-column prop="socialCode" label="社会统一代码" min-width="150">
            <template v-slot="scope">
              <el-input
                v-model="scope.row.socialCode"
                size="small"
                style="width: 100%"
                placeholder="社会统一代码"
                maxlength="18"
                clearable
              />
            </template>
          </el-table-column>
          <el-table-column prop="classification" label="供应商分类" min-width="180">
            <template v-slot="scope">
              <common-select
                v-model="scope.row.classification"
                :options="supplierClassEnum.ENUM"
                multiple
                clearable
                type="enum"
                placeholder="供应商分类"
                style="width: 100%"
                @change="handleSupplierClass($event, scope.row)"
              />
            </template>
          </el-table-column>
          <el-table-column label="操作" width="70px" align="center" fixed="right">
            <template v-slot="scope">
              <common-button
                type="danger"
                icon="el-icon-delete"
                size="mini"
                style="padding: 6px"
                @click.stop="removeRow(form.list, scope.$index)"
              />
            </template>
          </el-table-column>
        </common-table>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { computed, ref } from 'vue'

import { supplierClassEnum } from '@enum-ms/supplier'

import { regBatchForm } from '@compos/use-crud'
import useTableOperate from '@compos/form/use-table-operate'
import useTableValidate from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import StoreOperation from '@crud/STORE.operation'

const tableRules = {
  name: [{ min: 2, max: 32, message: '长度在 2 到 32 个字符', trigger: 'blur' }],
  classification: [{ required: true, message: '请选择供应商分类', trigger: 'blur' }]
}

const defaultForm = { list: [] }

const defaultRow = {
  name: '',
  classification: [],
  supplierClassification: undefined
}

const formRef = ref()

const { CRUD, crud, form, ADD_FORM } = regBatchForm(defaultForm, formRef)
const dialogVisible = computed(() => crud.bStatus.cu > CRUD.STATUS.NORMAL)

const { init, addRow, removeRow } = useTableOperate(defaultRow, 10)
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.supplier-batch-add',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false
  },
  dialogVisible
)

// 表格初始化
ADD_FORM.init = () => init(form.list)

// 表单校验
CRUD.HOOK.beforeValidateBCU = () => {
  const { validResult, dealList } = tableValidate(form.list)
  if (validResult) {
    form.list = dealList
  } else {
    return validResult
  }
}

// 表单提交数据清理
crud.submitBatchFormFormat = (form) => cleanUpData(form.list)

function handleSupplierClass(val, row) {
  let supplierClassification
  if (val) {
    val.forEach((v) => {
      supplierClassification |= v
    })
  }
  row.supplierClassification = supplierClassification
}
</script>
