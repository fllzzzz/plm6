<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelBCU"
    :visible="dialogVisible"
    :title="`${crud.props.factory ? crud.props.factory.name + '：' : ''}${crud.bStatus.title}`"
    :show-close="false"
    custom-class="drill-hole-config-batch-add"
    width="1000px"
    @opened="handleOpen"
    top="10vh"
  >
    <template #titleRight>
      <span style="float: right">
        <common-button size="mini" type="success" icon="el-icon-plus" @click="addRow(form.list)" />
        <common-button :loading="crud.bStatus.cu === CRUD.STATUS.PROCESSING" type="primary" size="mini" @click="crud.submitBCU">
          提 交
        </common-button>
        <store-operation type="crudBatch" />
        <common-button size="mini" @click="crud.cancelBCU">关 闭</common-button>
      </span>
    </template>
    <el-form ref="formRef" :model="form" :disabled="crud.bStatus.cu === CRUD.STATUS.PROCESSING">
      <common-table
        :data="form.list"
        :show-empty-symbol="false"
        return-source-data
        empty-text="暂无数据"
        v-loading="tableLoading"
        :max-height="maxHeight"
        :cell-class-name="wrongCellMask"
        row-key="uid"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="specPrefix" :show-overflow-tooltip="true" label="零件规格前缀" width="160">
          <template #default="{ row, $index }">
            <common-select
              v-model="row.specPrefix"
              :options="partKeyWordEnum.ENUM"
              clearable
              :show-extra="$index !== 0"
              type="enum"
              placeholder="零件规格前缀"
              style="width: 100%"
              class="input-underline"
            />
          </template>
        </el-table-column>
        <el-table-column prop="numThickness" :show-overflow-tooltip="true" label="板厚数值（毫米）" min-width="200" align="center">
          <template #default="{ row }">
            <common-input-number
              v-model="row.minThickness"
              :step="1"
              :min="0"
              :precision="2"
              clearable
              :controls="false"
              size="mini"
              class="input-underline"
              placeholder="最小板厚(毫米)"
              style="width: 45%"
            />
            <span> ~ </span>
            <common-input-number
              v-model="row.maxThickness"
              :step="1"
              :precision="2"
              class="input-underline"
              :controls="false"
              size="mini"
              clearable
              placeholder="最大板厚(毫米)"
              style="width: 45%"
            />
          </template>
        </el-table-column>
        <el-table-column prop="numBoreDiameter" :show-overflow-tooltip="true" label="孔径数值范围（毫米）" min-width="200" align="center">
          <template #default="{ row }">
            <common-input-number
              v-model="row.minBoreDiameter"
              :step="1"
              :min="0"
              :precision="2"
              clearable
              :controls="false"
              size="mini"
              class="input-underline"
              placeholder="最小孔径(毫米)"
              style="width: 45%"
            />
            <span> ~ </span>
            <common-input-number
              v-model="row.maxBoreDiameter"
              :step="1"
              :precision="2"
              class="input-underline"
              :controls="false"
              size="mini"
              clearable
              placeholder="最大孔径(毫米)"
              style="width: 45%"
            />
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="unitPrice" label="单价（元/个）" align="center" width="100px">
          <template #default="{ row }">
            <common-input-number
              v-model="row.unitPrice"
              :step="1"
              :min="0"
              :precision="2"
              clearable
              class="input-underline"
              :controls="false"
              size="mini"
              placeholder="单价"
              style="width: 100%"
            />
          </template>
        </el-table-column>
        <el-table-column label="操作" width="50" align="center">
          <template #default="{ $index }">
            <common-button
              type="danger"
              icon="el-icon-delete"
              size="mini"
              style="padding: 6px"
              @click.stop="removeRow(form.list, $index)"
            />
          </template>
        </el-table-column>
      </common-table>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { computed, ref, nextTick } from 'vue'
import { partKeyWordEnum } from '@enum-ms/mes'

import { regBatchForm } from '@compos/use-crud'
import useTableOperate from '@compos/form/use-table-operate'
import useTableValidate from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import StoreOperation from '@crud/STORE.operation.vue'

const validateNumThickness = (value, row) => {
  if (!row.minThickness || !row.maxThickness || row.maxThickness < row.minThickness) return false
  return true
}

const validateNumBoreDiameter = (value, row) => {
  if (!row.minBoreDiameter || !row.maxBoreDiameter || row.maxBoreDiameter < row.minBoreDiameter) return false
  return true
}

const tableRules = {
  specPrefix: [{ required: true, message: '请选择零件规格前缀', trigger: 'change' }],
  unitPrice: [{ required: true, message: '请填写单价', trigger: 'blur' }],
  numThickness: [{ validator: validateNumThickness, message: '请填写板厚并且最大板厚不得小于最小板厚', trigger: 'blur' }],
  numBoreDiameter: [{ validator: validateNumBoreDiameter, message: '请填写孔径并且最大孔径不得小于最小孔径', trigger: 'blur' }]
}

const defaultForm = { list: [] }

const defaultRow = {
  unitPrice: undefined
}

// 同上的选项与值
const ditto = new Map([['specPrefix', -1]])

const formRef = ref()
// 刷新表格
const tableLoading = ref(true)

const { CRUD, crud, form, ADD_FORM } = regBatchForm(defaultForm, formRef)
const dialogVisible = computed(() => crud.bStatus.cu > CRUD.STATUS.NORMAL)

const { init, addRow, removeRow } = useTableOperate(defaultRow, 5, ditto)
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.drill-hole-config-batch-add',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false
  },
  dialogVisible
)

ADD_FORM.init = () => init(form.list)

function handleOpen() {
  tableLoading.value = true
  nextTick(() => {
    tableLoading.value = false
  })
}

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
crud.submitBatchFormFormat = (form) => {
  cleanUpData(form.list)
  return form.list
}
</script>
