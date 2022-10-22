<template>
  <common-drawer
    ref="drawerRef"
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="库存明细"
    :wrapper-closable="false"
    size="80%"
    custom-class="steel-stock-detail"
  >
    <!-- <template #viewLeft>
    </template>  -->
    <template #content>
      <common-table
        ref="tableRef"
        border
        :data="list"
        :max-height="maxHeight"
        style="width: 100%"
        class="table-form"
        :stripe="false"
        return-source-data
        :showEmptySymbol="false"
      >
        <el-table-column label="序号" type="index" align="center" width="55" />
        <el-table-column key="name" prop="name" label="名称" show-overflow-tooltip align="left" />
        <el-table-column key="thickness" prop="thickness" :show-overflow-tooltip="true" label="规格" align="left">
          <template v-slot="scope">
            <span>{{ scope.row.thickness }}</span>
          </template>
        </el-table-column>
        <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" align="left">
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column key="quantity" prop="quantity" label="数量" show-overflow-tooltip align="left" />
        <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="长度(mm)" align="left">
          <template v-slot="scope">
            {{ scope.row.length ? scope.row.length.toFixed(DP.MES_ARTIFACT_L__MM) : '-' }}
          </template>
        </el-table-column>
        <el-table-column key="netWeight" prop="netWeight" label="重量(kg)" align="left" show-overflow-tooltip>
          <template v-slot="scope">
            {{ scope.row.netWeight ? scope.row.netWeight.toFixed(DP.COM_WT__KG) : '-' }}
          </template>
        </el-table-column>
        <el-table-column key="surfaceArea" prop="surfaceArea" label="库存" align="left" show-overflow-tooltip />
        <el-table-column key="drawingNumber" prop="drawingNumber" label="库位" align="left" show-overflow-tooltip>
          <template v-slot="scope">
            {{ scope.row.drawingNumber || '-' }}
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, defineEmits } from 'vue'

import useVisible from '@compos/use-visible'
import { DP } from '@/settings/config'
import useMaxHeight from '@compos/use-max-height'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  list: {
    type: Array,
    default: () => []
  }
})

const tableRef = ref()

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const drawerRef = ref()

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.steel-stock-detail',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300
  },
  () => drawerRef.value.loaded
)

</script>
<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
::v-deep(.el-dialog__body) {
  padding: 10px 20px;

  .el-step {
    .el-step__icon {
      width: 20px;
      height: 20px;
      font-size: 12px;
    }
    .el-step__title {
      font-size: 13px;
    }
  }
}
.tree-form {
  ::v-deep(.el-drawer__header) {
    margin-bottom: 0;
  }
}
.item-name {
  padding: 8px 16px;
  background-color: #ecf8ff;
  border-radius: 4px;
  border-left: 5px solid #50bfff;
  margin: 5px 0;
  margin-left: 5px;
  width: 150px;
}
.table-form {
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 5px;
  }
}
</style>

