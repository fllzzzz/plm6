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
    </template>
    <!-- 不刷新组件无法正常更新 -->
    <template v-if="dialogVisible">
      <el-form ref="formRef" :model="form" :disabled="cu.status.edit === FORM.STATUS.PROCESSING">
        <common-table
          :data="form.list"
          :max-height="maxHeight"
          show-summary
          :summary-method="getSummaries"
          :expand-row-keys="expandRowKeys"
          row-key="uid"
        >
          <!-- 次要信息：当列过多的时候，在展开处显示次要信息 -->
          <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" row-key="uid" fixed="left">
            <template #default="{ row }">
              <p>
                备注：<span v-empty-text>{{ row.remark }}</span>
              </p>
            </template>
          </el-expand-table-column>
          <!-- 基础信息 -->
          <material-base-info-columns :basic-class="props.basicClass" fixed="left" />
          <!-- 单位及其数量 -->
          <material-unit-quantity-columns :basic-class="props.basicClass" />
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
// import useTableValidate from '@/composables/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
// import useWmsConfig from '@/composables/store/use-wms-config'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import commonFooter from '@/views/wms/material-inbound/raw-material/application/components/common-footer.vue'

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
