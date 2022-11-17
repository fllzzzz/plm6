<template>
  <!-- 套料进度 -->
  <common-dialog width="100%" :show-close="false" modal fullscreen title="套料成果" append-to-body v-model="dialogVisible" :before-close="handleClose">
    <template #titleRight>
       <common-button @click.stop="handleClose" class="filter-item" type="success" size="mini">确认</common-button>
      <common-button @click.stop="delNesting" class="filter-item" type="danger" size="mini">删除</common-button>
    </template>
    <common-table
      v-loading="resultLoading"
      ref="tableRef"
      :data="nestingProgressData"
      :max-height="maxHeight + 160"
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
            <div style="width: 100%; display: flex">
              <template v-for="item in scope.row.linkDOList" :key="item">
                <el-tooltip effect="dark" :content="item.serialNumber" placement="top-start">
                  <div
                    :style="`padding: 0 5px; display:inline-block; width:${((item.length / scope.row.assembleLength) * 100).toFixed(
                      2
                    )}%; color: #fff; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; height: 30px; background-color: ${
                      item.lengthColor
                    };line-height: 30px; margin-left: ${(scope.row.kerfLength / scope.row.assembleLength) * 100}% ;margin-right: ${
                      (scope.row.kerfLength / scope.row.assembleLength) * 100
                    }%`"
                  >
                    <!-- 17dh13535487865887486 -->
                    {{ item.serialNumber }}
                  </div>
                </el-tooltip>
              </template>
              <el-tooltip v-if="scope.row.typesettingTypeEnum === nestingSettingTypeEnum.LOSSY.V" effect="dark" content="余料" placement="top-start">
                <div class="shadow" style="flex: 1"></div>
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
          <span v-if="scope.row.typesettingTypeEnum === nestingSettingTypeEnum.UN_LOSSY.V">-</span>
          <span v-else>{{ scope.row.length }}</span>
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
      <el-table-column key="netWeight" prop="netWeight" :show-overflow-tooltip="true" label="母材总重" align="center" width="150px">
        <template v-slot="scope">
          <span v-if="scope.row.typesettingTypeEnum === nestingSettingTypeEnum.UN_LOSSY.V">-</span>
          <span v-else>{{ scope.row.netWeight }}</span>
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
import { mesBuildingTypeSettingAssembleTypeEnum as materialTypeEnum, nestingSettingTypeEnum } from '@enum-ms/mes'
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
const { maxHeight } = useMaxHeight({
  mainBox: '.common-dialog',
  extraBox: ['.el-dialog__header'],
  wrapperBox: ['.el-dialog__body'],
  navbar: false,
  clientHRepMainH: true,
  minHeight: 300
})

const colorObj = ref({}) // serialNumber: color

// 套料成果
async function nestingResultGet() {
  try {
    resultLoading.value = true
    const { content } = await nestingProgress({ batchId: props.batchId })
    content[0].typesettingDTOS.forEach((v) => {
      v.assembleLength = v.typesettingTypeEnum === nestingSettingTypeEnum.UN_LOSSY.V ? 0 : v.length
      v.linkDOList.map((m) => {
        if (!colorObj.value[m.serialNumber]) {
          colorObj.value[m.serialNumber] = getLightColor()
        }
        m.lengthColor = colorObj.value[m.serialNumber]
        if (v.typesettingTypeEnum === nestingSettingTypeEnum.UN_LOSSY.V) {
          v.assembleLength += m.length
        }
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
.shadow {
  background: linear-gradient(135deg, #ffffff 25%, #5588aa 0, #5588aa 50%, #ffffff 0, #ffffff 75%, #5588aa 0);
  background-size: 15px 15px;
  color: #dbfaff;
  height: 30px;
  display: inline-block;
}
</style>
