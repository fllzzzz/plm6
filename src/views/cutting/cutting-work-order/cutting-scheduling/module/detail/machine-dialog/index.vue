<template>
  <common-dialog
    title="机器转产"
    width="50%"
    :show-close="false"
    :close-on-click-modal="false"
    v-model="drawerVisible"
    :before-close="handleClose"
  >
    <template #titleRight>
      <common-button type="primary" size="mini" @click="complete">完 成</common-button>
      <common-button size="mini" @click="handleClose">关 闭</common-button>
    </template>
    <div class="flex-rss">
      <el-tag
        :effect="selectLineId === item.mac ? 'light' : 'plain'"
        :type="selectLineId === item.mac ? 'success' : 'info'"
        v-for="item in machineData"
        class="elTag"
        hit
        @click="handleChange(item)"
        :key="item.id"
        >{{ item.machineName }}
        </el-tag>
    </div>
  </common-dialog>
</template>

<script setup>
import { defineProps, defineEmits, ref } from 'vue'
import { changeTask } from '@/api/cutting/machine'
import useVisible from '@compos/use-visible'
import { ElMessageBox, ElNotification } from 'element-plus'
import { getMachine } from '@/api/cutting/project-data'

const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
  detailData: {
    type: Object
  },
  selectLineId: {
    type: String,
    default: ''
  },
  isSingle: {
    type: Boolean,
    default: false
  }
})
const machineData = ref([]) // 获取所有机器。。。
const macValve = ref()
const emit = defineEmits(['change'], ['endEvent'])
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })
function showHook() {
  getReport()
}

async function getReport() {
  const { content } = await getMachine()
  machineData.value = content
}

function handleChange(item) {
  macValve.value = item
  emit('change', item)
}

async function complete() {
  try {
    await ElMessageBox.confirm('是否由' + " '" + macValve.value.machineName + "' " + '协同？', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    const list = [props.detailData.id]
    const message = await changeTask({ mac: macValve.value.mac }, list)
    ElNotification({ title: '更改状态成功', message: message, type: 'success' })
    emit('endEvent')
  } catch (err) {
    console.log(err)
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
.production-lines-content {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: center;
  flex-wrap: wrap;
  box-sizing: border-box;
  padding: 10px 0;
  .el-tag {
    width: 200px;
    max-width: 300px;
    text-align: center;
    margin: 0 15px 15px 0;
    cursor: pointer;
    text-overflow: ellipsis;
    white-space: nowrap;
    overflow: hidden;
  }
  .el-tag--info {
    border-color: #303133;
    color: #303133;
  }
}
.elTag {
  width: 200px;
  max-width: 300px;
  text-align: center;
  margin: 0 15px 15px 0;
  cursor: pointer;
  text-overflow: ellipsis;
  white-space: nowrap;
  overflow: hidden;
}
</style>

