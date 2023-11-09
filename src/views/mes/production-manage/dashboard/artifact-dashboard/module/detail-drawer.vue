 <template>
  <common-drawer
    ref="drawerRef"
    :title="`${
      detailRow.productType & componentTypeEnum.ARTIFACT.V
        ? detailRow.name + ' ' + detailRow.serialNumber + ' ' + commonQuantity
        : detailRow.serialNumber + ' ' + commonQuantity
    }`"
    v-model="detailVisible"
    direction="btt"
    :before-close="handleClose"
    :custom-class="'board-drawer'"
    size="60%"
  >
    <template #content>
      <el-row ref="contentTitle" class="content-title">
        <el-col :span="6" v-show="detailRow.productType & componentTypeEnum.ARTIFACT.V">名称：{{ detailRow.name || '-' }}</el-col>
        <el-col :span="6">编号：{{ detailRow.serialNumber || '-' }}</el-col>
        <el-col :span="6">规格：{{ detailRow.specification || '-' }}</el-col>
        <el-col :span="6">材质：{{ detailRow.material || '-' }}</el-col>
        <el-col :span="6">长度：{{ detailRow.length || '-' }} mm</el-col>
        <el-col :span="6">单净重：{{ detailRow.netWeight?.toFixed(DP.COM_WT__KG) || '-' }} kg</el-col>
        <el-col :span="6">单毛重：{{ detailRow.grossWeight?.toFixed(DP.COM_WT__KG) || '-' }} kg</el-col>
        <el-col :span="6" v-show="detailRow.productType & componentTypeEnum.ARTIFACT.V">图号：{{ detailRow.drawingNumber || '-' }}</el-col>
      </el-row>
      <div v-loading="loading" class="main-container">
        <template v-if="detailRow?.processSummaryDetailsList?.length">
          <el-radio-group v-model="processId" size="small" class="filter-item">
            <el-radio-button v-for="(item, index) in detailRow?.processSummaryDetailsList" :key="index" :label="index">
              {{ item.name }}({{ item.sumQuantity || 0 }})
            </el-radio-button>
          </el-radio-group>
          <div class="team-wrap" :style="{ maxHeight }">
            <div v-for="(item, index) in detailRow?.processSummaryDetailsList" v-show="processId === index" :key="index">
              <template v-if="item.teamTraceList?.length">
                <el-collapse v-for="team in item?.teamTraceList" :key="team.name">
                  <!-- <el-collapse-item
                    :title="`${detailRow?.processSummaryDetailsList[processId]?.productLineName}>${detailRow?.processSummaryDetailsList[processId]?.groupName}>${detailRow?.processSummaryDetailsList[processId]?.name} / ${team.userName}`"
                    :name="`${team.userId}`"
                  > -->
                  <el-collapse-item
                    :title="`${team?.productLineName}>${team?.groupName}>${detailRow?.processSummaryDetailsList[processId]?.name} / ${team.userName}`"
                    :name="`${team.userId}`"
                  >
                    <div style="display: flex">
                      <div>
                        <div
                          v-for="(produceTeam, produceTeamIndex) in team?.produceTeamRecourseList"
                          :key="produceTeamIndex"
                          class="team-item"
                        >
                          <div class="name">生产：{{ produceTeam?.userName || '-' }}</div>
                          <div class="quantity">数量：{{ produceTeam?.quantity || '-' }}</div>
                          <div class="time">时间：{{ parseTime(produceTeam?.createTime, '{y}-{m}-{d} {h}:{i}:{s}') || '-' }}</div>
                        </div>
                      </div>
                      <div style="margin-left: 200px">
                        <div
                          v-for="(inspectionTeam, inspectionTeamIndex) in team?.InspectionTeamRecourseList"
                          :key="inspectionTeamIndex"
                          class="team-item"
                        >
                          <div class="name">
                            <i
                              :class="[
                                inspectionTeam?.boolAdoptEnum ? 'is-success' : 'is-error',
                                inspectionTeam?.boolAdoptEnum ? 'el-icon-check' : 'el-icon-close',
                              ]"
                            />
                            质检：{{ inspectionTeam?.userName || '-' }}
                          </div>
                          <div class="quantity">数量：{{ inspectionTeam?.quantity || '-' }}</div>
                          <div class="time">时间：{{ parseTime(inspectionTeam?.createTime, '{y}-{m}-{d} {h}:{i}:{s}') || '-' }}</div>
                        </div>
                      </div>
                    </div>
                  </el-collapse-item>
                </el-collapse>
              </template>
              <span v-else class="red-tip">* 暂无数据</span>
            </div>
          </div>
        </template>
        <span v-else class="red-tip">* 暂无数据</span>
      </div>
    </template>
  </common-drawer>
</template>
<script setup>
import { ref, computed, defineProps, defineEmits } from 'vue'
import { DP } from '@/settings/config'
import { parseTime } from '@/utils/date'
import { componentTypeEnum } from '@enum-ms/mes'
import { ElRadioGroup, ElRadioButton, ElCollapse, ElCollapseItem } from 'element-plus'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const loading = ref(false)
const processId = ref(0)
const emit = defineEmits(['update:visible'])
const props = defineProps({
  detailRow: {
    type: Object,
    default: () => {}
  },
  visible: {
    type: Boolean,
    required: true
  }
})

const commonQuantity = computed(() => {
  return props.detailRow?.completeQuantity + '/' + props.detailRow?.compareQuantity
})

const { visible: detailVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.board-drawer',
    extraBox: ['.el-drawer__header', '.head-container'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300
  },
  drawerRef
)

function showHook() {
  processId.value = 0
  props.detailRow?.processSummaryDetailsList?.forEach(v => {
    v.teamTraceList?.forEach(o => {
      o.totalQuantity = o.produceTeamRecourseList?.reduce((pre, cur) => {
        if (cur) {
          return pre + cur?.quantity
        } else {
          return pre
        }
      }, 0)
    })
    v.sumQuantity = v.teamTraceList?.reduce((pre, cur) => {
      if (cur) {
        return pre + cur?.totalQuantity
      } else {
        return pre
      }
    }, 0)
  })
}
</script>

<style lang="scss" scoped>
.content-title {
  padding: 10px 20px;
  line-height: 30px;
  box-sizing: border-box;
}
.main-container {
  padding: 20px;
  .team-wrap {
    box-sizing: border-box;
    overflow-y: auto;
    .team-item {
      display: flex;
      padding-bottom: 10px;
      > div {
        margin-right: 10px;
        &.name {
          width: 180px;
        }
        &.quantity {
          width: 100px;
        }
        &.time {
          flex: 1;
        }
        i {
          border-radius: 100%;
          padding: 1px;
          margin-right: 4px;
          font-size: 13px;
          &.is-success {
            color: #67c23a;
            border: 1px solid #67c23a;
          }
          &.is-error {
            color: #f56c6c;
            border: 1px solid #f56c6c;
          }
        }
      }
    }
  }
}
</style>
