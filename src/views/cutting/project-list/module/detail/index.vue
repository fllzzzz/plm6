<template>
  <common-dialog
    :title="machineName"
    width="73%"
    :show-close="false"
    :close-on-click-modal="false"
    v-model="drawerVisible"
    :before-close="handleClose"
  >
    <template #titleRight>
      <common-button size="mini" @click="handleClose">关 闭</common-button>
    </template>

    <div class="flex-rss">
      <common-table v-loading="tabLoading" row-key="id" ref="tableRef" :max-height="500" style="width: 100%" :data="plateData">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="projectName" prop="projectName" :show-overflow-tooltip="true" label="所属项目" min-width="80">
          <template v-slot="scope">
            <span>{{ scope.row.projectName }}</span>
          </template>
        </el-table-column>
        <el-table-column key="cutInstructionId" prop="cutInstructionId" :show-overflow-tooltip="true" label="指令号" min-width="100">
          <template v-slot="scope">
            <span>{{ scope.row.cutInstructionId }}</span>
          </template>
        </el-table-column>
        <el-table-column key="material" align="center" prop="material" :show-overflow-tooltip="true" label="材质" min-width="45">
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column key="thick" prop="thick" :show-overflow-tooltip="true" label="厚度（mm）" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.thick }}</span>
          </template>
        </el-table-column>
        <el-table-column key="width" prop="width" :show-overflow-tooltip="true" label="宽度（mm）" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.width }}</span>
          </template>
        </el-table-column>
        <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="长度（mm）" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.length }}</span>
          </template>
        </el-table-column>
        <el-table-column key="weight" prop="weight" :show-overflow-tooltip="true" label="单重（kg）" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.weight }}</span>
          </template>
        </el-table-column>
        <el-table-column key="weight" prop="weight" :show-overflow-tooltip="true" label="总重（kg）" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.weight }}</span>
          </template>
        </el-table-column>

        <el-table-column key="plateState" prop="plateState" :show-overflow-tooltip="true" label="状态" min-width="55">
          <template v-slot="scope">
            <span>{{ steelPlateEnum.VL[scope.row.plateState] }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" label="暂停切割" min-width="60">
          <template v-slot="scope">
            <el-switch
              v-loading="switchLoading"
              v-model="scope.row.State"
              @change="StateChange(scope.row)"
              :disabled="
                !(
                  scope.row.plateState === '1' ||
                  scope.row.plateState === '0' ||
                  scope.row.plateState === '2' ||
                  scope.row.plateState === '5'
                )
              "
            />
          </template>
        </el-table-column>
        <el-table-column align="center" label="操作" min-width="80">
          <template v-slot="scope">
            <common-button type="danger" size="mini" @click="del(scope.row)">删除</common-button>
            <common-button type="warning" size="mini" @click="nestResults(scope.row)">协同</common-button>
          </template>
        </el-table-column>
      </common-table>
    </div>
  </common-dialog>

  <machine-dialog
    @endEvent="endEvent"
    :selectLineId="selectLineId"
    :detail-data="machieData"
    v-model:visible="machineVisible"
    @change="handleChange"
    isSingle
  />
</template>

<script setup>
import useVisible from '@compos/use-visible'
import { steelPlateEnum } from '@enum-ms/cutting'
import { defineProps, defineEmits, ref } from 'vue'
import { get } from '@/api/cutting/project-data'
import { suspendTask } from '@/api/cutting/scheduling'
import { continueTask } from '@/api/cutting/project-data'
import { ElNotification } from 'element-plus'
import machineDialog from './machine-dialog/index'

const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
  detailData: {
    type: Object
  }
})

const emit = defineEmits(['update:visible'])
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })
const plateData = ref([]) // 页面数据
const machineVisible = ref(false)
const machineName = ref('')
const tabLoading = ref(false)
const switchLoading = ref(false)
const selectLineId = ref('')
const machieData = ref()
function showHook() {
  if (props.detailData) {
    machineName.value = props.detailData.cutMachine.machineName
    plateDataGet()
  }
}

async function plateDataGet() {
  tabLoading.value = true
  try {
    const { content } = await get({ mac: props.detailData.cutMachine.mac })
    content.forEach(item => {
      if (item.plateState === '5') {
        item.State = true
      } else {
        item.State = false
      }
    })
    plateData.value = content
  } catch (err) {
    console.log('钢板清单页面接口报错', err)
  }
  tabLoading.value = false
}

function nestResults(row) {
  machieData.value = row
  selectLineId.value = row.mac
  machineVisible.value = true
}

function del(row) {
  console.log(row)
}

async function StateChange(row) {
  switchLoading.value = true
  try {
    const data = [row.id]
    let message = ''
    if (row.plateState === '5') {
      // 继续
      message = await continueTask(data)
      ElNotification({ title: '更改状态成功', message: message, type: 'success' })
    } else {
      // 暂停
      message = await suspendTask(data)
      ElNotification({ title: '更改状态成功', message: message, type: 'success' })
    }
  } catch (err) {
    console.log(err)
  }
  switchLoading.value = false
  plateDataGet()
}

function handleChange(row) {
  selectLineId.value = row.mac
  machieData.value = row
}

function endEvent() {
  machineVisible.value = false
  plateDataGet()
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

