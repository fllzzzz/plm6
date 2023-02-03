<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.status.cu > 0"
    :before-close="crud.cancelCU"
    :title="crud.status.title"
    :show-close="true"
    :size="700"
  >
    <template #titleRight>
      <common-button size="mini" type="success" icon="el-icon-plus" @click="addRow(form.list)" />
      <common-button :loading="crud.status.cu === 2" size="mini" type="primary" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <div class="process-main-content">
        <el-form ref="formRef" :model="form" size="small" label-width="80px">
          <el-form-item label="工序类型" prop="productType">
            <span v-parse-enum="{ e: typeEnum, v: form.productType }" />
          </el-form-item>
          <common-table
            border
            :data="form.list"
            :show-empty-symbol="false"
            return-source-data
            :cell-class-name="(data) => wrongCellMask(data, tableRules)"
            :max-height="maxHeight"
            row-key="uid"
          >
            <el-table-column label="序号" type="index" align="center" width="60" />
            <el-table-column prop="name" label="工序" align="center" min-width="180">
              <template #default="{ row }">
                <el-input
                  v-model.trim="row.name"
                  :readonly="row.boolUsed"
                  type="text"
                  placeholder="工序"
                  size="mini"
                  maxlength="20"
                  class="input-underline"
                  style="width: 100%"
                />
              </template>
            </el-table-column>
            <el-table-column prop="type" label="类型" align="center" min-width="180">
              <template #default="{ row }">
                <common-select
                  v-model="row.type"
                  class="input-underline"
                  :options="pcByProductTypeEnum"
                  type="enum"
                  placeholder="类型"
                  style="width: 100%"
                  :disabled="row.id"
                />
              </template>
            </el-table-column>
            <el-table-column label="操作" align="center" width="110">
              <template v-slot="scope">
                <common-button
                  v-if="!scope.row.boolUsed"
                  icon="el-icon-top"
                  type="primary"
                  style="padding: 5px"
                  size="mini"
                  :disabled="scope.$index === 0 || Boolean(scope.row.name === '下料' && form.productType & typeEnum.MACHINE_PART.V)"
                  @click="handleMove(scope, 'up', form.list)"
                />
                <common-button
                  v-if="!scope.row.boolUsed"
                  icon="el-icon-bottom"
                  type="primary"
                  style="padding: 5px"
                  size="mini"
                  :disabled="scope.$index === form.list.length - 1 || Boolean(scope.row.name === '下料' && form.productType & typeEnum.MACHINE_PART.V)"
                  @click="handleMove(scope, 'down', form.list)"
                />
                <common-button
                  v-if="!scope.row.boolUsed"
                  icon="el-icon-minus"
                  type="danger"
                  style="padding: 5px"
                  size="mini"
                  :disabled="Boolean(scope.row.name === '下料' && form.productType & typeEnum.MACHINE_PART.V)"
                  @click="removeRow(form.list, scope.$index)"
                />
                <svg-icon v-else class="icon icon-readonly" icon-class="readonly" />
              </template>
            </el-table-column>
          </common-table>
        </el-form>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { computed, ref } from 'vue'
import { processMaterialListTypeEnum as typeEnum, processCategoryEnum } from '@enum-ms/mes'

import { regForm } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useTableOperate from '@compos/form/use-table-operate'
import useTableValidate from '@compos/form/use-table-validate'
import SvgIcon from '@comp/SvgIcon/index.vue'

const defaultForm = {
  productType: typeEnum.ARTIFACT.V, // 工序类型
  list: [] // 具体工序列表
}

// from.list => 行数据
const defaultRow = {
  name: undefined
}

const pcByProductTypeEnum = computed(() => {
  if (form.productType & typeEnum.ARTIFACT.V) {
    return [processCategoryEnum.ASSEMBLY_RIVETING_WELDING, processCategoryEnum.PAINT]
  } else if (form.productType & typeEnum.ASSEMBLE.V) {
    return [processCategoryEnum.ASSEMBLY_RIVETING_WELDING]
  } else if (form.productType & typeEnum.MACHINE_PART.V) {
    return [processCategoryEnum.DRILL_HOLE, processCategoryEnum.MAKINGS]
  } else {
    return processCategoryEnum.ENUM
  }
})

const tableRules = {
  name: [{ required: true, message: '请输入工序', trigger: 'blur' }],
  type: [{ required: true, message: '请选择类型', trigger: 'change' }]
}

const drawerRef = ref()
const formRef = ref()

const { maxHeight } = useMaxHeight(
  {
    extraBox: '.el-drawer__header',
    wrapperBox: ['.el-drawer__body', '.process-main-content'],
    navbar: false,
    extraHeight: 70,
    minHeight: 300
  },
  () => drawerRef.value.loaded
)

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const { init, addRow, removeRow } = useTableOperate(defaultRow, 10)

// 初始化表单
CRUD.HOOK.afterToAdd = () => {
  init(form.list)
}
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules })

CRUD.HOOK.beforeValidateCU = () => {
  const { validResult, dealList } = tableValidate(form.list)
  if (validResult) {
    form.list = dealList
  } else {
    return validResult
  }
}

// 上下移动
function handleMove(scope, moveType, list) {
  const { $index } = scope
  if (moveType === 'up') {
    if ($index === 0) return
    const isUp = list[$index - 1]
    list.splice($index - 1, 1)
    list.splice($index, 0, isUp)
  } else {
    if ($index === list.length - 1) return
    const isDown = list[$index + 1]
    list.splice($index + 1, 1)
    list.splice($index, 0, isDown)
  }
}

// 表单提交数据清理
crud.submitFormFormat = (form) => {
  cleanUpData(form.list)
  form.list = form.list.map((v, index) => {
    // v.inspectType = inspectTypeEnum.BATCH_SCAN.V
    // v.reportType = reportTypeEnum.BATCH_SCAN.V
    // v.productType = form.productType
    v.sort = index + 1
    return v
  })
  return form
}
</script>

<style lang="scss" scoped>
.process-main-content {
  ::v-deep(.el-input-number .el-input__inner) {
    text-align: left;
  }
  ::v-deep(.el-button--mini) {
    min-height: 20px;
  }
}
</style>
