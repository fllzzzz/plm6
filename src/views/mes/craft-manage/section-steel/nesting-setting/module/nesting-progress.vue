<template>
  <!-- 套料进度 -->
  <common-dialog width="100%" modal fullscreen title="套料成果" append-to-body v-model="dialogVisible" :before-close="handleClose">
    <template #titleRight>
      <common-button @click.stop="delNesting" class="filter-item" type="danger" size="mini">删除</common-button>
    </template>
    <common-table v-loading="resultLoading" ref="tableRef" :data="nestingProgressData" :max-height="500" style="width: 100%" row-key="id">
      <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
      <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="套料编号" align="center" width="180px" fixed="left">
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        key="nestingResult"
        prop="nestingResult"
        label="套料成果"
        align="center"
      >
        <template v-slot="scope">
          <template v-if="scope.row.linkDOList.length > 0">
            <div style="display: flex;justify-content: flex-start;overflow: auto">
            <div v-for="item in scope.row.linkDOList" :key="item">
              <div :style="`width: ${35 * (scope.row.typesettingLength / item.length)}px; height: 30px; background-color: ${item.lengthColor};line-height: 30px;margin-left: 1px`">
                {{ item.serialNumber }}
              </div>
            </div>
            </div>
          </template>
        </template>
      </el-table-column>
      <el-table-column
        key="typesettingAssembleTypeEnum"
        prop="typesettingAssembleTypeEnum"
        :show-overflow-tooltip="true"
        label="材料属性"
        align="center"
        width="120px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.typesettingAssembleTypeEnum ? materialTypeEnum.VL[scope.row.typesettingAssembleTypeEnum] : '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="母材长度（mm）" align="center" width="120px">
        <template v-slot="scope">
          <span>{{ scope.row.length }}</span>
        </template>
      </el-table-column>
      <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="母材规格" align="center" width="120px">
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量" align="center" width="120px">
        <template v-slot="scope">
          <span>{{ scope.row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column key="typesettingLength" prop="typesettingLength" :show-overflow-tooltip="true" label="套料长度（mm）" align="center" width="120px">
        <template v-slot="scope">
          <span>{{ scope.row.typesettingLength }}</span>
        </template>
      </el-table-column>
      <el-table-column key="lossRate" prop="lossRate" :show-overflow-tooltip="true" label="损耗" align="center" width="120px">
        <template v-slot="scope">
          <span>{{ scope.row.lossRate }}</span>
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>
<script setup>
import { ref, defineProps, defineEmits } from 'vue'
import { nestingProgress, delNestingResult } from '@/api/mes/craft-manage/section-steel/nesting-setting'
import { mesBuildingTypeSettingAssembleTypeEnum as materialTypeEnum } from '@enum-ms/mes'
import { ElMessageBox, ElNotification } from 'element-plus'
import { getLightColor } from '@/utils/color'
import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:visible'])
const nestingProgressData = ref([])
const resultLoading = ref(false)
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  batchId: {
    type: Number,
    default: undefined
  }
})
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: nestingResultGet })

const colorObj = ref({}) // serialNumber: color

// 套料成果
async function nestingResultGet() {
  try {
    resultLoading.value = true
    const { content } = await nestingProgress({ batchId: props.batchId })
    content[0].typesettingDTOS.forEach((v) => {
      v.linkDOList.map((m) => {
        if (!colorObj.value[m.serialNumber]) {
          colorObj.value[m.serialNumber] = getLightColor()
        }
        m.lengthColor = colorObj.value[m.serialNumber]
      })
    })

    nestingProgressData.value = content[0].typesettingDTOS
  } catch (error) {
    console.log('获取套料成果失败')
  } finally {
    resultLoading.value = false
  }
}
async function delNesting() {
  try {
    ElMessageBox.confirm(`是否确认删除套料成果`, '提示', {
      confirmButtonText: '确认',
      cancelButtonText: '取消',
      type: 'warning'
    }).then(async () => {
      try {
        const _data = []
        _data.push(props.batchId)
        await delNestingResult(_data)
        ElNotification({
          title: '删除成功',
          type: 'success',
          duration: 2500
        })
        handleClose()
        emit('success')
      } catch (error) {
        console.log('删除失败', error)
      }
    })
  } catch (er) {
    console.log(er, '删除失败')
  }
}
</script>

<style lang="scss" scoped>

</style>
