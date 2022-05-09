<template>
  <common-dialog width="60%" :title="'所选设备: ' + machieName" append-to-body v-model="drawerVisible" :before-close="closeDialog"  :close-on-click-modal="false">
    <template #titleRight>
      <common-button size="mini" @click="taskIssue"  type="success">任务分配</common-button>
    </template>

    <div class="flex-rss">
      <common-table ref="tableRef" :data="detailData" :max-height="500" style="width: 100%" row-key="id">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="cutInstructionId" prop="cutInstructionId" :show-overflow-tooltip="true" label="指令号" min-width="100" align="center" >
          <template v-slot="scope">
            <span>{{ scope.row.cutInstructionId }}</span>
          </template>
        </el-table-column>
        <el-table-column key="material" align="center" prop="material" :show-overflow-tooltip="true" label="材质" min-width="40">
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column key="thick" prop="thick" :show-overflow-tooltip="true" label="厚度（mm）" min-width="40" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.thick }}</span>
          </template>
        </el-table-column>
        <el-table-column key="width" prop="width" :show-overflow-tooltip="true" label="宽度（mm）" min-width="40" align="center" >
          <template v-slot="scope">
            <span>{{ scope.row.width }}</span>
          </template>
        </el-table-column>
        <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="长度（mm）" min-width="40" align="center" >
          <template v-slot="scope">
            <span>{{ scope.row.length }}</span>
          </template>
        </el-table-column>
        <el-table-column key="num" prop="num" :show-overflow-tooltip="true" label="数量" min-width="30" align="center" >
          <template v-slot="scope">
            <span>{{ scope.row.num }}</span>
          </template>
        </el-table-column>
      </common-table>
    </div>
  </common-dialog>
</template>

<script setup>
import useVisible from '@compos/use-visible'
import { get } from '@/api/cutting/project-data'

import { defineProps, defineEmits, ref } from 'vue'

const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
  detailData: {
    type: Array,
    required: true
  },
  selectLine: {
    type: Object,
    required: true
  }
})

const emit = defineEmits(['update:visible'], ['taskIssue'])

const machieName = ref('')

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', closeHook, showHook })

function showHook() {
  machieName.value = props.selectLine.machineName
 
}

function closeHook() {
  emit('closeHook')
  
}

function closeDialog(){
  handleClose()

}
function taskIssue() {
  handleClose()
  emit('taskIssue')
}

</script>

<style rel="stylesheet/scss" lang="scss" scoped>
.item-name {
  padding: 8px 16px;
  background-color: #ecf8ff;
  border-radius: 4px;
  border-left: 5px solid rgb(81, 113, 131);
  margin: 10px 0;
  margin-left: 5px;
  width: 150px;
}
.TaskPackage {
  margin-top: 30px;
}
.title-style {
  font-weight: 700;
  font-size: 18px;
  color: #000;
}
</style>

