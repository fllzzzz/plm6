<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelBCU"
    :visible="dialogVisible"
    :title="crud.bStatus.title"
    :show-close="false"
    custom-class="mes-cutting-config"
    width="700px"
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
    <div>
      <el-form ref="formRef" :model="form" :disabled="crud.bStatus.cu === CRUD.STATUS.PROCESSING">
        <common-table
          :data="form.list"
          :show-empty-symbol="false"
          return-source-data
          empty-text="暂无数据"
          :max-height="maxHeight"
          default-expand-all
          :cell-class-name="wrongCellMask"
          style="width: 100%"
        >
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column key="type" prop="type" label="切割形式" align="center" min-width="180">
            <template #default="{ row, $index }">
              <common-select
                :key="Math.random()"
                v-model="row.cutType"
                :options="cuttingConfigEnum.ENUM"
                :show-extra="$index !== 0"
                type="enum"
                placeholder="切割形式"
                style="width: 100%"
              />
            </template>
          </el-table-column>
          <el-table-column key="thickness" prop="thickness" :show-overflow-tooltip="true" align="center" label="厚度" min-width="120">
            <template #default="{ row }">
              <common-input-number
                v-model="row.thickness"
                :min="0"
                :max="999999"
                controls-position="right"
                :controls="false"
                :precision="2"
                size="mini"
                style="width: 100%"
                placeholder="厚度"
              />
            </template>
          </el-table-column>
          <el-table-column
            key="boolDrillEnum"
            prop="boolDrillEnum"
            :show-overflow-tooltip="true"
            align="center"
            label="是否切孔"
            width="120"
          >
            <template #default="{ row }">
              <el-switch
                v-model="row.boolDrillEnum"
                :active-value="whetherEnum.TRUE.V"
                :inactive-value="whetherEnum.FALSE.V"
                class="drawer-switch"
              />
            </template>
          </el-table-column>
          <el-table-column label="操作" width="70px" align="center">
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
    </div>
  </common-dialog>
</template>

<script setup>
import { computed, ref } from 'vue'
import { cuttingConfigEnum } from '@enum-ms/mes'
import { whetherEnum } from '@enum-ms/common'

import { regBatchForm } from '@compos/use-crud'
import useTableOperate from '@compos/form/use-table-operate'
import useTableValidate from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import StoreOperation from '@crud/STORE.operation.vue'

const tableRules = {
  thickness: [{ required: true, message: '请填写厚度', trigger: 'blur' }],
  symbol: [{ max: 3, message: '不能超过3个字符', trigger: 'blur' }],
  cutType: [{ required: true, message: '请选择切割形式', trigger: 'change' }]
}

const defaultForm = { list: [] }

const defaultRow = {
  name: undefined,
  symbol: undefined,
  boolDrillEnum: whetherEnum.FALSE.V,
  cutType: undefined
}

// 同上的选项与值
const ditto = new Map([['cutType', -1]])

const formRef = ref()

const { CRUD, crud, form, ADD_FORM } = regBatchForm(defaultForm, formRef)
const dialogVisible = computed(() => crud.bStatus.cu > CRUD.STATUS.NORMAL)

const { init, addRow, removeRow } = useTableOperate(defaultRow, 10, ditto)
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.mes-cutting-config',
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
</script>
