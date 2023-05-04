<template>
  <common-dialog
    custom-class="inbound-application-preview"
    title="提交预览"
    append-to-body
    v-model="dialogVisible"
    width="1200px"
    :before-close="handleClose"
    :top="'5vh'"
    fullscreen
  >
    <template #titleAfter>
      <span>项目:<span>{{globalProject.serialNumber}}</span><span style="margin-left:5px;">{{globalProject.shortName}}</span></span>
      <span v-if="isNotBlank(currentMonomer)" style="margin-left:10px;">单体:{{currentMonomer.name}}</span>
      <span v-if="isNotBlank(currentArea)" style="margin-left:10px;">区域:{{currentArea.name}}</span>
    </template>
    <!-- 不刷新组件无法正常更新 -->
    <template v-if="dialogVisible">
      <el-form ref="formRef" :model="form" :disabled="cu.status.edit === FORM.STATUS.PROCESSING">
        <common-table
          :data="form.list"
          :max-height="maxHeight"
          show-summary
          :summary-method="getSummaries"
          return-source-data
          :showEmptySymbol="false"
        >
          <!-- 基础信息 -->
          <el-table-column prop="serialNumber" label="编号" align="center" width="110px" fixed="left" show-overflow-tooltip />
          <el-table-column
            prop="classifyName"
            label="名称"
            align="center"
            fixed="left"
            min-width="150"
            show-overflow-tooltip
          >
            <template #default="{ row }">
              <el-tooltip :content="row.classifyParentFullName" :disabled="!row.classifyParentFullName" :show-after="500" placement="top">
                <span v-empty-text="row.classifyName" />
              </el-tooltip>
            </template>
          </el-table-column>
          <el-table-column prop="specification" label="规格" align="center" min-width="200px" fixed="left" show-overflow-tooltip>
            <template #default="{ row }">
              <el-tooltip :content="row.specificationLabels" placement="top">
                <span v-empty-text="row.specification" />
              </el-tooltip>
            </template>
          </el-table-column>
          <el-table-column prop="measureUnit" label="计量单位" align="center" min-width="70px">
            <template #default="{ row }">
              <span v-empty-text>{{ row.measureUnit }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="quantity" label="数量" align="center" min-width="120px">
            <template #default="{ row }">
              <span v-empty-text>{{ row.quantity }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="accountingUnit" label="核算单位" align="center" min-width="70px">
            <template #default="{ row }">
              <span v-empty-text>{{ row.accountingUnit }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="mete" label="核算量" align="center" min-width="120px">
            <template #default="{ row }">
              <span v-empty-text>{{ row.mete }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="color" label="颜色" align="center" min-width="120px">
            <template #default="{ row }">
              <span v-empty-text>{{ row.color }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="brand" label="品牌" align="center" min-width="120px">
            <template #default="{ row }">
              <span v-empty-text>{{ row.brand }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="useProperty" label="使用范围" align="center">
            <template #default="{ row }">
              <span v-empty-text>{{ auxiliaryMaterialUseTypeEnum.VL[row.useProperty] }}</span>
            </template>
          </el-table-column>
        </common-table>
      </el-form>
    </template>
    <common-footer class="footer" :show-total="false" is-submit />
  </common-dialog>
</template>

<script setup>
import { defineEmits, defineProps, ref, inject } from 'vue'
import { tableSummary } from '@/utils/el-extra'
import { isNotBlank } from '@/utils/data-type'

import { regExtra } from '@/composables/form/use-form'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import commonFooter from '@/views/wms/material-inbound/raw-material/application/components/common-footer.vue'
import { auxiliaryMaterialUseTypeEnum } from '@enum-ms/plan'

const emit = defineEmits(['saveSuccess', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  },
  basicClass: {
    type: Number
  }
})

const expandRowKeys = ref([]) // 展开行key

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, closeHook: closeHook })
const { cu, form, FORM } = regExtra() // 表单

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.inbound-application-preview',
    extraBox: ['.el-dialog__header', '.logistics-form-content', '.footer'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    extraHeight: 10
  },
  dialogVisible
)
const currentMonomer = inject('currentMonomer')
const currentArea = inject('currentArea')
const globalProject = inject('globalProject')
// 表单提交后：关闭预览窗口
FORM.HOOK.afterSubmit = () => {
  handleClose()
}

function closeHook() {
  // 关闭窗口时，取消所有选中，避免再次打开，数据无法及时更新
  expandRowKeys.value = []
}

// 合计
function getSummaries(param) {
  return tableSummary(param, { props: ['quantity', 'mete'] })
}
</script>

<style lang="scss" scoped>
.inbound-application-preview {
  position: relative;
  .el-dialog__header .el-tag {
    min-width: 70px;
  }

  .logistics-form-content {
    padding: 0 30px;
    position: absolute;
    bottom: 50px;
    left: 0;
  }
  .footer {
    position: absolute;
    bottom: 0;
    left: 0;
  }
}
</style>
