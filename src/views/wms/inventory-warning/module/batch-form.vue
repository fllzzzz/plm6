<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelBCU"
    :visible="dialogVisible"
    :title="crud.bStatus.title"
    :show-close="false"
    fullscreen
    custom-class="inventory-warning-batch-add"
    width="700px"
    top="10vh"
  >
    <template #titleRight>
      <common-button size="mini" type="success" icon="el-icon-plus" @click="addRow(form.list)" />
      <common-button :loading="crud.bStatus.cu === CRUD.STATUS.PROCESSING" type="primary" size="mini" @click="crud.submitBCU">
        提 交
      </common-button>
      <!-- <store-opertaion type="crudBatch" /> -->
      <common-button size="mini" @click="crud.cancelBCU">关 闭</common-button>
    </template>
    <div class="flex-rsc">
      <div class="setting-item">
        <el-tag size="medium" effect="plain">科目</el-tag>
        <material-cascader
          v-model="form.classifyId"
          separator=" > "
          show-all-levels
          clearable
          size="small"
          style="width: 250px"
          @change="handleClassifyChange"
        />
      </div>
      <div class="setting-item">
        <el-tag size="medium" effect="plain">【批量】设置预警数量</el-tag>
        <el-input-number
          v-model.number="form.batchNumber"
          :min="0"
          :max="999999"
          :step="100"
          size="small"
          controls-position="right"
          placeholder="预警数量"
        />
        <common-button type="success" size="small">设置</common-button>
      </div>
      <div class="setting-item">
        <el-tag size="medium" effect="plain">【批量】设置工厂</el-tag>
        <factory-select
          v-model:value="form.batchFactory"
          placeholder="可选择工厂"
          clearable
        />
        <common-button type="success" size="small">设置</common-button>
      </div>
    </div>
    <div class="container flex-rbs" style="margin-top: 20px; align-items: flex-start">
      <div class="flex-r" style="width: 100%">
        <common-table ref="tableRef" border :data="selectList" style="width: 100%">
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column key="subjectFullName" :show-overflow-tooltip="true" prop="subjectFullName" label="科目" align="left" />
          <el-table-column key="content" :show-overflow-tooltip="true" prop="content" label="规格" align="left" />
          <el-table-column key="unitType" :show-overflow-tooltip="true" prop="unitType" label="单位配置" align="center" width="140">
            <template v-slot="scope">
              <common-radio-button v-model:value="scope.row.unitType" :options="measureTypeEnum" type="enum" size="small" />
            </template>
          </el-table-column>
          <el-table-column key="unit" :show-overflow-tooltip="true" prop="unit" label="单位" align="center" width="70">
            <template v-slot="scope">
              {{ scope.row.unitType === measureTypeEnum.MEASUREMENT_UNIT.V ? scope.row.unit[0] : scope.row.unit[1] }}
            </template>
          </el-table-column>
          <el-table-column key="number" :show-overflow-tooltip="true" prop="number" label="数量" align="center" width="120">
            <template v-slot="scope">
              <el-input-number
                v-model.number="scope.row.number"
                :min="0"
                :max="999999"
                :step="100"
                size="small"
                controls-position="right"
                style="width: 100%; max-width: 200px"
              />
            </template>
          </el-table-column>
          <el-table-column key="factory" prop="factory" label="工厂" align="center" width="160">
            <template v-slot="scope">
              <factory-select v-model:value="scope.row.factory" placeholder="可选择工厂" clearable style="width: 100%" />
            </template>
          </el-table-column>
          <el-table-column key="operate" :show-overflow-tooltip="true" prop="operate" label="操作" align="center" width="70">
            <template v-slot="scope">
              <common-button icon="el-icon-delete" type="danger" size="mini" @click="scope.row.selected = !scope.row.selected" />
            </template>
          </el-table-column>
        </common-table>
      </div>
    </div>
  </common-dialog>
</template>

<script setup>
import { ref, computed } from 'vue'
import { measureTypeEnum } from '@enum-ms/wms'

import { regBatchForm } from '@compos/use-crud'
import useTableValidate from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import materialCascader from '@comp-cls/material-cascader/index.vue'
import factorySelect from '@comp-mes/factory-select/index.vue'
// import subscribeList from './subscribe-list'
// import factorySelect from '@/views/components/base/factory-select'

const tableRules = {
  // name: [{ required: true, max: 20, message: '不能超过20个字符', trigger: 'blur' }],
  // symbol: [{ max: 3, message: '不能超过3个字符', trigger: 'blur' }],
  // type: [{ required: true, message: '请选择单位类型', trigger: 'change' }]
}

const defaultForm = {
  id: undefined,
  name: null,
  pid: 1
}

const tableRef = ref()
const formRef = ref()

const { CRUD, crud, form, ADD_FORM } = regBatchForm(defaultForm, formRef)
const dialogVisible = computed(() => crud.bStatus.cu > CRUD.STATUS.NORMAL)

// const { init, addRow, removeRow } = useTableOperate(defaultRow, 10, dittos)
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules })

// const { maxHeight } = useMaxHeight(
//   {
//     mainBox: '.unit-batch-add',
//     extraBox: ['.el-dialog__header'],
//     wrapperBox: ['.el-dialog__body'],
//     clientHRepMainH: true,
//     navbar: false
//   },
//   dialogVisible
// )

// 表格初始化
// ADD_FORM.init = () => init(form.list)

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

<style lang="scss" scoped>
.setting-item {
  display: flex;
  align-items: center;
  .el-tag.el-tag--medium {
    height: 32px;
    line-height: 28px;
  }
}

.setting-item + .setting-item {
  margin-left: 30px;
}
.setting-item > :nth-child(n) {
    margin-right: 6px;
}
.setting-item > :last-child {
    margin-right: 0px;
}
</style>
