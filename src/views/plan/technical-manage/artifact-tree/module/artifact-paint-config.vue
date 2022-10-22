<template>
  <common-drawer
    ref="drawerRef"
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="油漆涂装工艺配置"
    :wrapper-closable="false"
    size="85%"
    custom-class="paint-config"
  >
    <template #titleRight>
      <common-button type="success" size="mini" :loading="loading">保存</common-button>
    </template>
    <template #content>
      <common-table
        ref="tableRef"
        border
        :data="[{}]"
        :max-height="maxHeight"
        style="width: 100%"
        class="table-form"
        :stripe="false"
        return-source-data
        :showEmptySymbol="false"
      >
        <el-table-column label="序号" type="index" align="center" width="55" />
        <el-table-column key="serialNumber" prop="serialNumber" label="构件类型" show-overflow-tooltip />
        <el-table-column key="quantity" prop="quantity" label="数量" show-overflow-tooltip />
        <el-table-column key="netWeight" prop="netWeight" :label="`重量(t)`" align="left" show-overflow-tooltip>
          <template v-slot="scope">
            {{ scope.row.netWeight ? scope.row.netWeight.toFixed(DP.COM_WT__T) : '-' }}
          </template>
        </el-table-column>
        <el-table-column key="area" prop="area" :show-overflow-tooltip="true" :label="`油漆面积(㎡)`" align="left">
          <template v-slot="scope">
            {{ scope.row.area ? scope.row.area.toFixed(DP.COM_AREA__M2) : '-' }}
          </template>
        </el-table-column>
        <el-table-column key="drawingNumber" prop="drawingNumber" label="底漆(μm)" align="left" show-overflow-tooltip width="280">
          <template v-slot="scope">
            <div style="display:float;">
              <el-input-number
                v-model.number="scope.row.thickness"
                :min="0"
                :max="99999999999"
                :step="1"
                :precision="DP.COM_T__MM"
                :controls="false"
                placeholder="底漆"
                class="input-underline"
                style="width: 50%;float:left;"
              />
              <common-radio v-model="scope.row.paintType" :options="paintTypeEnum.ENUM" type="enum" style="float:right;border-left:1px solid #ccc;"/>
            </div>
          </template>
        </el-table-column>
        <el-table-column key="drawingNumber" prop="drawingNumber" label="中间漆(μm)" align="left" show-overflow-tooltip width="280">
          <template v-slot="scope">
            <div style="display:float;">
              <el-input-number
                v-model.number="scope.row.thickness"
                :min="0"
                :max="99999999999"
                :step="1"
                :precision="DP.COM_T__MM"
                :controls="false"
                placeholder="中间漆"
                class="input-underline"
                style="width: 50%;float:left;"
              />
              <common-radio v-model="scope.row.paintType" :options="paintTypeEnum.ENUM" type="enum" style="float:right;border-left:1px solid #ccc;"/>
            </div>
          </template>
        </el-table-column>
        <el-table-column key="drawingNumber" prop="drawingNumber" label="面漆(μm)" align="left" show-overflow-tooltip width="280">
          <template v-slot="scope">
            <div style="display:float;">
              <el-input-number
                v-model.number="scope.row.thickness"
                :min="0"
                :max="99999999999"
                :step="1"
                :precision="DP.COM_T__MM"
                :controls="false"
                placeholder="面漆"
                class="input-underline"
                style="width: 50%;float:left;"
              />
              <common-radio v-model="scope.row.paintType" :options="paintTypeEnum.ENUM" type="enum" style="float:right;border-left:1px solid #ccc;"/>
            </div>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, defineEmits, watch } from 'vue'
// import crudApi from '@/api/plan/technical-manage/artifact-tree'

import useVisible from '@compos/use-visible'
import { paintTypeEnum } from '@enum-ms/plan'
// import { ElNotification } from 'element-plus'
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

watch(
  () => props.modelValue,
  (val) => {
    if (val) {
      if (tableRef.value) {
        tableRef.value.clearSelection()
      }
    }
  },
  { deep: true, immediate: true }
)

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const loading = ref(false)
const drawerRef = ref()

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.paint-config',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300
  },
  () => drawerRef.value.loaded
)

// async function deleteItems() {
//   loading.value = true
//   try {
//     await crudApi.del(listSelection.value)
//     loading.value = false
//     handleSuccess()
//   } catch (e) {
//     loading.value = false
//     console.log('删除构件', e)
//   }
// }

// function handleSuccess() {
//   ElNotification({ title: '删除成功', type: 'success' })
//   emit('success')
//   handleClose()
// }

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
  ::v-deep(.el-radio){
    display:block;
  }
  ::v-deep(.el-radio.el-radio--small){
    height:23px;
  }
}

</style>

