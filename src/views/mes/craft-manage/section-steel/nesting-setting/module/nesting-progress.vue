<template>
  <!-- 套料进度 -->
  <common-dialog width="100%" modal fullscreen title="套料成果" append-to-body v-model="dialogVisible" :before-close="handleClose">
    <template #titleRight>
      <common-button @click.stop="delNesting" class="filter-item" type="danger" size="mini">删除</common-button>
    </template>
    <common-table
      v-loading="resultLoading"
      ref="tableRef"
      :data="nestingProgressData"
      :max-height="maxHeight"
      style="width: 100%"
      row-key="id"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="套料编号" align="center" width="180px">
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column key="nestingResult" prop="nestingResult" label="套料成果" header-align="center">
        <template v-slot="scope">
          <template v-if="scope.row.linkDOList.length > 0">
            <div style="width: 100%; display: inline-block">
              <template v-for="item in scope.row.linkDOList" :key="item">
                <el-tooltip effect="dark" :content="item.serialNumber" placement="top-start">
                  <div
                    :style="`padding: 0 5px; display:inline-block; width:${
                      (item.length / scope.row.length) * 100
                    }%; color: #fff; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; height: 30px; background-color: ${
                      item.lengthColor
                    };line-height: 30px; border-right: 1px solid #fff;`"
                  >
                    <!-- 17dh13535487865887486 -->
                    {{ item.serialNumber }}
                  </div>
                </el-tooltip>
              </template>
              <el-tooltip effect="dark" content="余料" placement="top-start">
                <div
                  :style="`display:inline-block; width: ${scope.row.lossRate}%; color: #fff;overflow: hidden; text-overflow: ellipsis; white-space: nowrap; height: 30px; background-color: #5e5d5d;line-height: 30px;`"
                ></div>
              </el-tooltip>
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
      <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="母材长度（mm）" align="center" width="150px">
        <template v-slot="scope">
          <span>{{ scope.row.length }}</span>
        </template>
      </el-table-column>
      <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="母材规格" align="center" width="150px">
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" align="center" width="110px">
        <template v-slot="scope">
          <span>{{ scope.row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column
        key="netWeight"
        prop="netWeight"
        :show-overflow-tooltip="true"
        label="母材总重"
        align="center"
        width="150px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.netWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量" align="center" width="120px">
        <template v-slot="scope">
          <span>{{ scope.row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column
        key="typesettingLength"
        prop="typesettingLength"
        :show-overflow-tooltip="true"
        label="套料长度（mm）"
        align="center"
        width="150px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.typesettingLength }}</span>
        </template>
      </el-table-column>
      <el-table-column key="lossRate" prop="lossRate" :show-overflow-tooltip="true" label="损耗" align="center" width="80px">
        <template v-slot="scope">
          <span>{{ scope.row.lossRate }}%</span>
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
import useMaxHeight from '@compos/use-max-height'

const emit = defineEmits(['update:visible', 'success'])
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

// 高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.common-dialog',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300
  }
)

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
        emit('success')
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
